/*
 * Framebuffer Kernel Log Driver
 * Copyright (c) 2012 David Herrmann <dh.herrmann@googlemail.com>
 */

/*
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

/*
 * Framebuffer Kernel Log
 * This driver prints the kernel log to all connected display devices. It
 * replaces CONFIG_VT and cannot run simultaneously with it. It does not provide
 * any virtual-terminal, though. It should only be used to get kernel boot
 * messages to debug kernel errors.
 * Hence, this driver is neither optimized for speed, nor does it provide any
 * fancy features like colored text output. After booting is done, the init
 * process should set /sys/class/graphics/fblog/active to 0 which disables this
 * driver and you can start using the graphics devices. During shutdown, you can
 * set this to 1 to get log messages again.
 * This driver forcibly writes to the framebuffer while active, therefore, you
 * cannot run other graphics applications simultaneously.
 *
 * fblog_redraw_line() is heavily based on the fbcon driver. See bitblit.c for
 * the original implementation copyrighted by:
 *     Copyright (C) 2004 Antonino Daplas <adaplas@pol.net>
 *
 * Please note that nearly all functions here must be called with console_lock
 * held. This way, we have no locking problems and do not need special
 * synchronization.
 */

#include <linux/atomic.h>
#include <linux/console.h>
#include <linux/fb.h>
#include <linux/font.h>
#include <linux/module.h>

#define FBLOG_STR(x) x, sizeof(x) - 1

enum fblog_flags {
	FBLOG_KILLED,
	FBLOG_SUSPENDED,
	FBLOG_BLANKED,
};

/**
 * struct fblog_buf: Console text buffer
 *
 * Each framebuffer has its own text buffer which contains all characters that
 * are currently printed on screen. The buffers might have different sizes and
 * can be resized during runtime. When the buffer content changes, we redraw the
 * screen.
 *
 * width: Width of buffer in characters
 * height: Height of buffer in characters
 * lines: Array of lines
 * pos_x: Cursor x-position
 * pos_y: Cursor y-position
 */
struct fblog_buf {
	size_t width;
	size_t height;
	char **lines;
	size_t pos_x;
	size_t pos_y;
};

/**
 * struct fblog_fb: Framebuffer object
 *
 * For each framebuffer we register this object. It contains all data we need to
 * display the console log on it. The index of a framebuffer in registered_fb[]
 * is the same as in fblog_fbs[]. So the following must always be true if the
 * pointers are non-NULL:
 *     registered_fb[idx] == fblog_fbs[idx]->info
 *     fblog_fbs[idx]->info->node == idx
 *
 * flags: Framebuffer flags (see fblog_flags)
 * info: Pointer to the associated framebuffer device
 * font: Currently used font
 * buf: Console text buffer
 */
struct fblog_fb {
	unsigned long flags;
	struct fb_info *info;
	const struct font_desc *font;
	struct fblog_buf buf;
};

static struct fblog_fb *fblog_fbs[FB_MAX];
static struct device *fblog_device;
static atomic_t fblog_active;

static void fblog_buf_resize(struct fblog_buf *buf, size_t width,
			     size_t height)
{
	char **lines = NULL;
	size_t i, j, minw, minh;

	if (buf->height == height && buf->width == width)
		return;

	if (width && height) {
		lines = kzalloc(height * sizeof(char*), GFP_KERNEL);
		if (!lines)
			return;

		for (i = 0; i < height; ++i) {
			lines[i] = kzalloc(width * sizeof(char), GFP_KERNEL);
			if (!lines[i]) {
				while (i--)
					kfree(lines[i]);
				return;
			}
		}

		/* copy old lines */
		minw = min(width, buf->width);
		minh = min(height, buf->height);
		if (height >= buf->height)
			i = 0;
		else
			i = buf->height - height;

		for (j = 0; j < minh; ++i, ++j)
			memcpy(lines[j], buf->lines[i], minw * sizeof(char));
	} else {
		width = 0;
		height = 0;
	}

	for (i = 0; i < buf->height; ++i)
		kfree(buf->lines[i]);
	kfree(buf->lines);

	buf->lines = lines;
	buf->width = width;
	buf->height = height;
}

static void fblog_buf_init(struct fblog_buf *buf)
{
	fblog_buf_resize(buf, 80, 24);
}

static void fblog_buf_deinit(struct fblog_buf *buf)
{
	fblog_buf_resize(buf, 0, 0);
}

static void fblog_buf_rotate(struct fblog_buf *buf)
{
	char *line;

	if (!buf->height)
		return;

	line = buf->lines[0];
	memset(line, 0, sizeof(char) * buf->width);

	memmove(buf->lines, &buf->lines[1], sizeof(char*) * (buf->height - 1));
	buf->lines[buf->height - 1] = line;
}

static void fblog_buf_write(struct fblog_buf *buf, const char *str, size_t len)
{
	char c;

	if (!buf->height)
		return;

	while (len--) {
		c = *str++;

		if (c == '\n') {
			buf->pos_x = 0;
			if (++buf->pos_y >= buf->height) {
				buf->pos_y = buf->height - 1;
				fblog_buf_rotate(buf);
			}
		} else if (c == 0) {
			/* ignore */
		} else {
			if (buf->pos_x >= buf->width) {
				buf->pos_x = 0;
				++buf->pos_y;
			}
			if (buf->pos_y >= buf->height) {
				buf->pos_y = buf->height - 1;
				fblog_buf_rotate(buf);
			}

			buf->lines[buf->pos_y][buf->pos_x++] = c;
		}
	}
}

static void fblog_redraw_aligned(struct fblog_fb *fb, const char *s, u32 cnt,
				 u32 d_pitch, u32 s_pitch, u32 cellsize,
				 struct fb_image *image, u8 *dst)
{
	struct fb_info *info = fb->info;
	const struct font_desc *font = fb->font;
	u32 idx = font->width >> 3;
	u8 *src;

	while (cnt--) {
		src = (void*)(font->data + (*s++ & 0xff) * cellsize);
		fb_pad_aligned_buffer(dst, d_pitch, src, idx, image->height);
		dst += s_pitch;
	}

	info->fbops->fb_imageblit(info, image);
}

static void fblog_redraw_unaligned(struct fblog_fb *fb, const char *s, u32 cnt,
				   u32 d_pitch, u32 s_pitch, u32 cellsize,
				   struct fb_image *image, u8 *dst)
{
	struct fb_info *info = fb->info;
	const struct font_desc *font = fb->font;
	u32 shift_low = 0, mod = font->width % 8;
	u32 shift_high = 8;
	u32 idx = font->width >> 3;
	u8 *src;

	while (cnt--) {
		src = (void*)(font->data + (*s++ & 0xff) * cellsize);
		fb_pad_unaligned_buffer(dst, d_pitch, src, idx,
					image->height, shift_high,
					shift_low, mod);
		shift_low += mod;
		dst += (shift_low >= 8) ? s_pitch : s_pitch - 1;
		shift_low &= 7;
		shift_high = 8 - shift_low;
	}

	info->fbops->fb_imageblit(info, image);
}

static void fblog_redraw_line(struct fblog_fb *fb, size_t line,
			      const char *str, size_t len)
{
	struct fb_info *info = fb->info;
	const struct font_desc *font = fb->font;
	struct fb_image image;
	u32 width = DIV_ROUND_UP(font->width, 8);
	u32 cellsize = width * font->height;
	u32 maxcnt = info->pixmap.size / cellsize;
	u32 scan_align = info->pixmap.scan_align - 1;
	u32 buf_align = info->pixmap.buf_align - 1;
	u32 mod = font->width % 8;
	u32 cnt, pitch, size;
	u8 *dst;

	image.fg_color = 7;
	image.bg_color = 0;
	image.dx = 0;
	image.dy = line * font->height;
	image.height = font->height;
	image.depth = 1;

	while (len) {
		if (len > maxcnt)
			cnt = maxcnt;
		else
			cnt = len;

		image.width = font->width * cnt;
		pitch = DIV_ROUND_UP(image.width, 8) + scan_align;
		pitch &= ~scan_align;
		size = pitch * image.height + buf_align;
		size &= ~buf_align;
		dst = fb_get_buffer_offset(info, &info->pixmap, size);
		image.data = dst;

		if (!mod)
			fblog_redraw_aligned(fb, str, cnt, pitch, width,
					     cellsize, &image, dst);
		else
			fblog_redraw_unaligned(fb, str, cnt, pitch, width,
					       cellsize, &image, dst);

		image.dx += cnt * font->width;
		len -= cnt;
		str += cnt;
	}
}

static void fblog_redraw_clear(struct fblog_fb *fb)
{
	struct fb_fillrect region;
	struct fb_info *info = fb->info;

	region.color = 0;
	region.dx = 0;
	region.dy = 0;
	region.width = info->var.xres;
	region.height = info->var.yres;
	region.rop = ROP_COPY;

	info->fbops->fb_fillrect(info, &region);
}

static void fblog_redraw(struct fblog_fb *fb)
{
	size_t i, len;

	if (!fb || !fb->font || test_bit(FBLOG_KILLED, &fb->flags) ||
	    test_bit(FBLOG_SUSPENDED, &fb->flags) ||
	    test_bit(FBLOG_BLANKED, &fb->flags))
		return;

	fblog_redraw_clear(fb);

	for (i = 0; i < fb->buf.height; ++i) {
		len = strnlen(fb->buf.lines[i], fb->buf.width);
		if (len)
			fblog_redraw_line(fb, i, fb->buf.lines[i], len);
	}
}

static struct fblog_fb *fblog_info2fb(struct fb_info *info)
{
	if (!info || info->node < 0 || info->node >= FB_MAX ||
	    !registered_fb[info->node])
		return NULL;

	return fblog_fbs[info->node];
}

static void fblog_register(struct fb_info *info)
{
	struct fblog_fb *fb;
	struct fb_var_screeninfo var;
	const struct fb_videomode *mode;
	unsigned int width, height;

	if (!atomic_read(&fblog_active))
		return;
	if (!info || info->node < 0 || info->node >= FB_MAX)
		return;
	if (!registered_fb[info->node] || fblog_fbs[info->node])
		return;

	fb = kzalloc(sizeof(*fb), GFP_KERNEL);
	if (!fb)
		return;

	fblog_fbs[info->node] = fb;
	fb->info = info;
	fblog_buf_init(&fb->buf);
	fblog_buf_write(&fb->buf, FBLOG_STR("Framebuffer log initialized\n"));

	if (!try_module_get(info->fbops->owner))
		goto out_killed;
	if (info->fbops->fb_open && info->fbops->fb_open(info, 0))
		goto out_unref;

	var = info->var;
	mode = fb_find_best_mode(&var, &info->modelist);
	var.activate = FB_ACTIVATE_NOW | FB_ACTIVATE_FORCE;
	fb_set_var(info, &var);

	fb->font = get_default_font(info->var.xres, info->var.yres,
				    info->pixmap.blit_x,
				    info->pixmap.blit_y);
	if (fb->font) {
		width = info->var.xres / fb->font->width;
		height = info->var.yres / fb->font->height;
		fblog_buf_resize(&fb->buf, width, height);
		fblog_redraw(fb);
	}

	return;

out_unref:
	module_put(info->fbops->owner);
out_killed:
	set_bit(FBLOG_KILLED, &fb->flags);
}

static void fblog_unregister(struct fblog_fb *fb)
{
	struct fb_info *info;

	if (!fb)
		return;

	info = fb->info;
	if (!test_bit(FBLOG_KILLED, &fb->flags)) {
		if (info->fbops->fb_release)
			info->fbops->fb_release(info, 0);
		module_put(info->fbops->owner);
	}

	fblog_buf_deinit(&fb->buf);
	fblog_fbs[info->node] = NULL;
	kfree(fb);
}

static void fblog_register_all(void)
{
	int i;

	for (i = 0; i < FB_MAX; ++i)
		fblog_register(registered_fb[i]);
}

static void fblog_unregister_all(void)
{
	int i;

	for (i = 0; i < FB_MAX; ++i)
		fblog_unregister(fblog_info2fb(registered_fb[i]));
}

static void fblog_refresh(struct fblog_fb *fb)
{
	unsigned int width, height;

	if (!fb || !fb->font)
		return;

	width = fb->info->var.xres / fb->font->width;
	height = fb->info->var.yres / fb->font->height;
	fblog_buf_resize(&fb->buf, width, height);
	fblog_redraw(fb);
}

static void fblog_activate(void)
{
	if (atomic_read(&fblog_active))
		return;

	atomic_set(&fblog_active, 1);
	fblog_register_all();
}

static void fblog_deactivate(void)
{
	if (!atomic_read(&fblog_active))
		return;

	atomic_set(&fblog_active, 0);
	fblog_unregister_all();
}

static int fblog_event(struct notifier_block *self, unsigned long action,
		       void *data)
{
	struct fb_event *event = data;
	struct fb_info *info = event->info;
	struct fblog_fb *fb = fblog_info2fb(info);
	int *blank;

	if (action == FB_EVENT_FB_REGISTERED) {
		/* This is called when a low-level system driver registers a new
		 * framebuffer. The registration lock is held but the console
		 * lock might not be held when this is called (really?). */
		fblog_register(info);
		return 0;
	}

	if (!fb)
		return 0;

	switch(action) {
	case FB_EVENT_FB_UNREGISTERED:
		/* This is called when a low-level system driver unregisters a
		 * framebuffer. The registration lock is held but the console
		 * lock might not be held (really?). */
		/* ignore; see UNBIND */
		break;
	case FB_EVENT_FB_UNBIND:
		/* Called directly before unregistering an FB. The FB is still
		 * valid here and the registration lock is held but the console
		 * lock might not be held (really?). */
		fblog_unregister(fb);
		break;
	case FB_EVENT_SUSPEND:
		/* This is called when the low-level display driver suspends the
		 * video system. We should not access the video system while it
		 * is suspended. This is called with the console lock held. */
		set_bit(FBLOG_SUSPENDED, &fb->flags);
		break;
	case FB_EVENT_RESUME:
		/* This is called when the low-level display driver resumes
		 * operating. It is called with the console lock held. */
		clear_bit(FBLOG_SUSPENDED, &fb->flags);
		break;
	case FB_EVENT_MODE_DELETE:
		/* This is sent when a video mode is removed. The current video
		 * mode is never removed! The console lock is held while this is
		 * called. */
		/* fallthrough */
	case FB_EVENT_NEW_MODELIST:
		/* This is sent when the modelist got changed. The console-lock
		 * is held and we should reset the mode. */
		/* fallthrough */
	case FB_EVENT_MODE_CHANGE_ALL:
		/* This is the same as below but notifies us that the user used
		 * the FB_ACTIVATE_ALL flag when setting the video mode. */
		/* fallthrough */
	case FB_EVENT_MODE_CHANGE:
		/* This is called when the _user_ changes the video mode via
		 * ioctls. It is not sent, when the kernel changes the mode
		 * internally. This callback is called inside fb_set_var() so
		 * the console lock is held. */
		fblog_refresh(fb);
		break;
	case FB_EVENT_BLANK:
		/* This gets called _after_ the framebuffer was successfully
		 * blanked. The console-lock is always held while fb_blank is
		 * called and during this callback. */
		blank = (int*)event->data;
		if (*blank == FB_BLANK_UNBLANK)
			clear_bit(FBLOG_BLANKED, &fb->flags);
		else
			set_bit(FBLOG_BLANKED, &fb->flags);
		break;
	case FB_EVENT_GET_REQ:
		/* When fb_set_var() is called, this callback is called to get
		 * our display requirements. They are then compared with the
		 * display properties and only if they fulfill the requirements,
		 * the new mode is activated. The console-lock should be held
		 * while calling fb_set_var() so we can assume it is locked
		 * here. */
		/* ignore */
		break;
	case FB_EVENT_CONBLANK:
		/* This is sent by fbcon when doing a fake blank. That
		 * is, blanking the screen when the fb driver failed to perform
		 * an fb_blank(). It simply writes empty lines to the screen.
		 * We are not interested in this signal. We should also never
		 * run together with fbcon so this should never be caught. */
		/* ignore */
		break;
	case FB_EVENT_GET_CONSOLE_MAP:
		/* fallthrough */
	case FB_EVENT_SET_CONSOLE_MAP:
		/* Is there any reason why we should support this? We
		 * ignore it as we consider ourself not to be the classic linux
		 * console. Hence, this request is not targeted at us. */
		/* ignore */
		break;
	case FB_EVENT_REMAP_ALL_CONSOLE:
		/* What are we supposed to do here? Do we have to remap
		 * the primary device to the framebuffer given by \info? Like
		 * above we currently ignore it for the same reasons. */
		/* ignore */
		break;
	}

	return 0;
}

static struct notifier_block fblog_notifier = {
	.notifier_call = fblog_event,
};

static void fblog_con_write(struct console *con, const char *buf,
			    unsigned int len)
{
	int i;

	for (i = 0; i < FB_MAX; ++i) {
		if (fblog_fbs[i]) {
			fblog_buf_write(&fblog_fbs[i]->buf, buf, len);
			fblog_redraw(fblog_fbs[i]);
		}
	}
}

static struct console fblog_con_driver = {
	.name = "fblog",
	.write = fblog_con_write,
	.flags = CON_PRINTBUFFER | CON_ENABLED,
};

static ssize_t fblog_dev_active_show(struct device *dev,
				     struct device_attribute *attr,
				     char *buf)
{
	return snprintf(buf, PAGE_SIZE, "%d\n", atomic_read(&fblog_active));
}

static ssize_t fblog_dev_active_store(struct device *dev,
				      struct device_attribute *attr,
				      const char *buf,
				      size_t count)
{
	unsigned long num;

	num = simple_strtoul(buf, NULL, 10);
	console_lock();
	if (num)
		fblog_activate();
	else
		fblog_deactivate();
	console_unlock();

	return count;
}

static DEVICE_ATTR(active, S_IRUGO | S_IWUSR | S_IWGRP, fblog_dev_active_show,
		   fblog_dev_active_store);

static void fblog_dev_release(struct device *dev)
{
	kfree(dev);
	module_put(THIS_MODULE);
}

static int __init fblog_init(void)
{
	int ret;

	fblog_device = kzalloc(sizeof(*fblog_device), GFP_KERNEL);
	if (!fblog_device) {
		pr_err("fblog: cannot allocate device\n");
		ret = -ENOMEM;
		goto err_out;
	}

	__module_get(THIS_MODULE);
	device_initialize(fblog_device);
	fblog_device->class = fb_class;
	fblog_device->release = fblog_dev_release;
	dev_set_name(fblog_device, "fblog");

	ret = device_add(fblog_device);
	if (ret) {
		pr_err("fblog: cannot add device\n");
		goto err_dev;
	}

	ret = fb_register_client(&fblog_notifier);
	if (ret) {
		pr_err("fblog: cannot register framebuffer notifier\n");
		goto err_dev_rm;
	}

	ret = device_create_file(fblog_device, &dev_attr_active);
	if (ret) {
		pr_err("fblog: cannot create sysfs entry\n");
		goto err_fb;
	}

	register_console(&fblog_con_driver);

	console_lock();
	fblog_activate();
	console_unlock();

	return 0;

err_fb:
	fb_unregister_client(&fblog_notifier);
err_dev_rm:
	device_del(fblog_device);
err_dev:
	put_device(fblog_device);
err_out:
	return ret;
}

static void __exit fblog_exit(void)
{
	unregister_console(&fblog_con_driver);
	device_remove_file(fblog_device, &dev_attr_active);
	device_del(fblog_device);
	fb_unregister_client(&fblog_notifier);
	console_lock();
	fblog_deactivate();
	console_unlock();
	put_device(fblog_device);
}

module_init(fblog_init);
module_exit(fblog_exit);
MODULE_LICENSE("GPL");
MODULE_AUTHOR("David Herrmann <dh.herrmann@googlemail.com>");
MODULE_DESCRIPTION("Framebuffer Kernel Log Driver");
