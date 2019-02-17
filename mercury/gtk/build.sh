LDFLAGS=`pkg-config gtk+-3.0 gio-2.0 epoxy --libs` \
CFLAGS=`pkg-config gtk+-3.0 gio-2.0 epoxy --cflags` \
mmc gtk.m --auto-comments
