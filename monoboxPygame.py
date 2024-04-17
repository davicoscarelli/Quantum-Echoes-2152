# coding: utf-8

_version_major=0
_version_minor=3
_version_patch=0
_version=(_version_major,_version_minor,_version_patch)
_author='Jirka Justra'
_date='201905'
_license='MIT expat'


#č

#~ Some useful demo widgets in pygame.

from monobox import Monobox

import pygame


def _pygame_txt_renderer(C,_c):
	c=_c['c']
	x=_c['x']
	y=_c['y']
	sel=_c['sel']
	
	if c=='\n': return
	clr=(0, 255, 0)
	if sel: clr=(255, 255, 0)
	w=C.cW
	h=C.cH
	lnW=0
	if not c:
		if C.show_cursor:
			x-=1
			y-=1
			w+=1
			h+=1
			lnW=1
			pygame.draw.rect(C.surf, clr, pygame.Rect(x, y, w,h),lnW)
	else:
		# cache key is combination of size and color
		k=(C.size,c,clr)
		# create character surface and store in cache
		if k not in C.cSurfCache:
			surf=C.font.render(c,0,clr)
			C.cSurfCache[k]=surf
		surf=C.cSurfCache[k]
		C.surf.blit(surf,(x,y))

class Textbox(Monobox):
	def __init__(C,rect=(0,0,100,100),size=20,cback=0,surf=None):
		C.rect_full=rect
		x,y,w,h=rect
		rect=(x+2,y+2,w-4,h-4)
		
		C.size=size
		C.font=pygame.font.Font(pygame.font.match_font('monospace'), size)
		
		if surf is None: surf=pygame.display.get_surface()
		C.surf=surf
		
		Monobox.__init__(C,rect,cback)
		
		C.wrap=1
		# cache for character surfaces
		# you can store some bitmap font here ^.^
		C.cSurfCache={}
		C.drag=0
		C.show_cursor=1
		C.show_border=1
		
		C.resize(w,h)
	def move(C,x,y):
		Monobox.move(C,x,y)
		
		_x,_y,_w,_h=C.rect
		C.rect_full=(_x-2,_y-2,_w+4,_h+4)
	def resize(C,w,h):
		Monobox.resize(C,w,h)
		
		_x,_y,_w,_h=C.rect
		C.rect_full=(_x-2,_y-2,_w+4,_h+4)
		
		C.cW,C.cH=C.font.size('a')# measure char size
		
		if C.rect[3]<=C.cH:
			x,y,w,h=C.rect
			C.rect=(x,y,w,C.cH+2)
			x,y,w,h=C.rect_full
			C.rect_full=(x,y,w,C.cH+2+8)
		
	def update(C,events):
		for event in events:
			if event.type == pygame.MOUSEMOTION:
				if not C.drag: continue
				x,y=event.pos
				x-=C.rect[0]
				y-=C.rect[1]
				C.curI=C.xy2i(x,y)
				if C.curI>=len(C.buff): C.curI=len(C.buff)-1
				C.unselect(*C.drag)# clear me
				C.drag[1]=C.curI# update me
				C.select(*C.drag)# now select me again
				# scroll it and
				# refresh cursor position
				C.refresh()
			if event.type == pygame.MOUSEBUTTONUP:
				if event.button==1: C.drag=0
			if event.type == pygame.MOUSEBUTTONDOWN:
				if event.button==1:
					x,y=event.pos
					if not pygame.Rect(C.rect).collidepoint(x,y): continue
					C.unselect()# clear previous selection - can be skipped for multi select
					x-=C.rect[0]
					y-=C.rect[1]
					C.curI=C.xy2i(x,y)
					if C.curI>=len(C.buff): C.curI=len(C.buff)-1
					C.drag=[C.curI,C.curI]
					C.refresh()# refresh cursor position
				# mouse scroll
				if event.button==4: C.up()
				if event.button==5: C.down()
			if event.type == pygame.KEYDOWN:
				if event.mod==64: continue# all ctrl+? combos
				
				if event.key==8: C.backspace()
				if event.key==13: C.add('\n')
				if event.key==127: C.delete()

				if event.key==276: C.left()
				if event.key==275: C.right()
				if event.key==273: C.up()
				if event.key==274: C.down()

				if event.key<256:
					c=chr(event.key)
					if c  in ' qwertyuiopasdfghjklzxcvbnm1234567890[];\',./{}:\\"!@#$%^&*()-=_+|<>?`~':
						C.add(event.unicode)
	def render(C,fc=None):
		if fc is None: fc=_pygame_txt_renderer
		if C.show_border: pygame.draw.rect(C.surf, (0,255,0), pygame.Rect(C.rect_full),1)
		Monobox.render(C,fc)


class Entry(Textbox):
	def __init__(C,rect=(0,0,100,100),size=20,cback=0,surf=None):
		#~ def _cback(C,c): return C.entry_inner_callback(c,cback)
		Textbox.__init__(C,rect,size,C.entry_inner_callback,surf)
		C._outer_callback=cback
		C.wrap=0
	def entry_inner_callback(C,c,cback):
		# this do oneline functionality
		# it is up to callback to call .toString() to get entry text
		if C._outer_callback:
			ret=C._outer_callback(C,c)
			if ret is not None: return ret
		if c=='\n':
			C.empty()
			return 0# reject \n char
		return 1# accept char

class Label(Textbox):
	def __init__(C,rect=(0,0,100,100),size=20,surf=None):
		Textbox.__init__(C,rect,size,0,surf)
		C.show_cursor=0
		C.show_border=0

