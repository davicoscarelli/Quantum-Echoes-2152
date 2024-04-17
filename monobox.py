# coding: utf-8

_version_major=0
_version_minor=3
_version_patch=0
_version=(_version_major,_version_minor,_version_patch)
_author='Jirka Justra'
_date='201905'
_license='MIT expat'


#č

#~ General class Monobox, whitch implements what is needed
#~ for toolkit-independent monospace-font textbox functionality.

class Monobox():
	def __init__(C,rect=(0,0,100,100),cback=0):
		C.rect=rect
		C.cback=cback
		
		# character dimensions
		C.cW=10
		C.cH=10
		# x and y margin
		C.cOffX=1
		C.cOffY=2
		C.wrap=0# wrap long lines
		# x and y scroll in box
		C.scrollX=0
		C.scrollY=0
		
		# index in C.buff where cursor currently is
		C.curI=0
		
		# list of characters and their properties
		C.buff=0
		# list of lines and indexes in C.buff where their begins
		C.lnIL=0
		
		# ini both above
		C.empty()


#
# tool functions
#
	
	# calculate character dimensions including margins
	def get_cW(C): return (C.cW+C.cOffX*1)
	def get_cH(C): return (C.cH+C.cOffY*1)
	
	# find how many can you fit in box
	def get_maxLnI(C): return int(C.rect[3]/C.get_cH())
	def get_maxCI(C):  return int(C.rect[2]/C.get_cW())
	
	
#
# conversion functions
#
	
	# buff index to lnI+cI
	def i2lnIcI(C,i):
		#~ if i>=len(C.buff): i=len(C.buff)-1
		if len(C.buff)==1: return 0,0
		if i>=len(C.buff):# for curI mainly
			i=len(C.buff)-1
			cI =C.buff[i]['cI']
			lnI=C.buff[i]['lnI']
			if cI>=C.get_maxCI():
				lnI+=1
				cI=0
			else:
				cI+=1
		else:
			cI =C.buff[i]['cI']
			lnI=C.buff[i]['lnI']
		return lnI,cI
	# lnI+cI to buff index
	def lnIcI2i(C,lnI,cI):
		if lnI<0: lnI=0
		if lnI>=len(C.lnIL): lnI=len(C.lnIL)-1
		i=C.lnIL[lnI]
		while i<len(C.buff):
			if C.buff[i]['lnI']!=lnI:
				i-=1
				break
			if C.buff[i]['cI']==cI: break
			i+=1
		return i

	# lnI+cI to box x+y
	def lnIcI2xy(C,lnI,cI):
		x=C.get_cW()*(cI -C.scrollX)
		y=C.get_cH()*(lnI-C.scrollY)
		return x,y
	# box x+y to lnI+cI
	def xy2lnIcI(C,x,y):
		cI= int(x/C.get_cW())+C.scrollX
		lnI=int(y/C.get_cH())+C.scrollY
		return lnI,cI

	# buff index to box x+y
	def i2xy(C,i):
		lnI,cI=C.i2lnIcI(i)
		return C.lnIcI2xy(lnI,cI)
	# box x+y to buff index
	def xy2i(C,x,y):
		lnI,cI=C.xy2lnIcI(x,y)
		return C.lnIcI2i(lnI,cI)


#
#
#

	def see(C):
		if not C.buff: return
		i=C.curI
		if i>=len(C.buff): i=len(C.buff)-1
		if i<0: i=0
		if C.buff[i]['lnI']<C.scrollY:
			C.scrollY=C.buff[i]['lnI']
		if C.buff[i]['lnI']>=C.scrollY+C.get_maxLnI():
			C.scrollY=C.buff[i]['lnI']-C.get_maxLnI()+1
		if C.buff[i]['cI']<C.scrollX:
			C.scrollX=C.buff[i]['cI']
		if C.buff[i]['cI']>=C.scrollX+C.get_maxCI():
			C.scrollX=C.buff[i]['cI']-C.get_maxCI()+1

	def move(C,x,y):
		_x,_y,w,h=C.rect
		C.rect=(x,y,w,h)
		C.refresh()
	def resize(C,w,h):
		x,y,_w,_h=C.rect
		C.rect=(x,y,w,h)
		C.refresh()
	
	# recalculate character properties
	# (line index, character/column index, etc.)
	#~ def refresh_c(C,i,k,v): pass
	def refresh(C,i=0):
		#~ if len(C.buff)==1: return# only cursor char is there
		lnI=0
		cI=0
		C.lnIL=[0]
		i-=1
		#~ for i,c in enumerate(C.buff):
		while i>=-1 and i<=len(C.buff)-2:
			i+=1
			c=C.buff[i]
			c['cI']=cI
			if C.wrap and cI>=C.get_maxCI():# wrap longs ?
				# this 'if' do word wraping;
				# comment out for dirty line wraping
				if c['c']!=' ':
					_i=i
					while _i>=0:
						_i-=1
						if C.buff[_i]['lnI']!=lnI: break
						if C.buff[_i]['c']==' ':# found space in current line
							i=_i+1
							c=C.buff[i]
							break
				lnI+=1
				cI=0
				c['cI']=cI
				C.lnIL.append(i)
			if c['c']=='\n':# EOL
				c['lnI']=lnI
				c['cI']=cI
				lnI+=1
				cI=0
				C.lnIL.append(i+1)
				continue
			else:
				cI+=1
			c['lnI']=lnI
			#~ c['_cX']=C.rect[0]+C.cOffX+c['cI'] *C.get_cW()
			#~ c['_cY']=C.rect[1]+C.cOffY+c['lnI']*C.get_cH()
		C.see()
		if C.scrollY<len(C.lnIL):# do we see any lines ?
			i=C.lnIL[C.scrollY]
			for i in range(i,len(C.buff)-1):# skip last char - cursor
				# skip invisible columns
				if C.scrollX>C.buff[i]['cI']:
					# this char will not be rendered
					C.buff[i]['render']=0
					continue
				
				cI =(C.buff[i]['cI'] -C.scrollX)
				lnI=(C.buff[i]['lnI']-C.scrollY)
				
				# skip invisible lines
				if lnI<0 or lnI>=C.get_maxLnI() or cI >=C.get_maxCI():
					# this char will not be rendered
					C.buff[i]['render']=0
					continue
				
				# this char will be rendered
				C.buff[i]['render']=1
				# calculate character position and store it
				cX=C.rect[0]+C.cOffX+cI *C.get_cW()
				cY=C.rect[1]+C.cOffY+lnI*C.get_cH()
				C.buff[i]['x']=cX
				C.buff[i]['y']=cY
		# cursor
		cX,cY=C.i2xy(C.curI)
		cX=C.rect[0]+C.cOffX+cX
		cY=C.rect[1]+C.cOffY+cY
		C.buff[-1]['x']=cX
		C.buff[-1]['y']=cY
	
	# callback fc : fc(C,c)
	def render(C,fc):
		if C.scrollY<len(C.lnIL):# do we see any lines ?
			i=C.lnIL[C.scrollY]
			
			for i in range(i,len(C.buff)-1):# skip last char - cursor
				if not C.buff[i]['render']: continue
				fc(C,C.buff[i])
		# cursor
		fc(C,C.buff[-1])
	
	
	# text editing
	
	def addC(C,c):
		if C.cback and not C.cback(C,c): return
		C.buff.insert(C.curI,{
			'c':c,
			'sel':0,# is selected ?
			'lnI':0,
			'cI':0,
			#~ '':,
		})
		C.curI+=1
	def add(C,s):
		for c in list(s): C.addC(c)
		C.refresh()
	def empty(C):
		# for cursor function : every line must end with [return], whole buffer with empty char
		C.lnIL=[0]
		C.buff=[{'c':'','sel':0,'lnI':0,'cI':0}]
		C.curI=0
	def set(C,s):
		C.empty()
		C.add(s)
	
	
	# text selection
	
	def sel(C,val,i0=-1,i1=-1):# both points inclusive
		_i0=min(i0,i1)
		_i1=max(i0,i1)
		for i,c in enumerate(C.buff):
			if _i0>=0 and i<_i0: continue
			if _i1>=0 and i>_i1: continue
			c['sel']=val

	def select(C,*a,**kw): return C.sel(1,*a,**kw)
	def unselect(C,*a,**kw): return C.sel(0,*a,**kw)

	def toString(C,selection_only=0):
		l=[]
		for c in C.buff:
			if not c['sel'] and selection_only: continue
			l.append(c['c'])
		return ''.join(l)


#
# key press functions
#
	
	# arrow keys
	
	def left(C):
		if C.curI==0: return
		C.curI-=1
		C.refresh()
	def right(C):
		if C.curI>=len(C.buff)-1: return
		C.curI+=1
		C.refresh()
	def up(C):
		lnI,cI=C.i2lnIcI(C.curI)
		C.curI=C.lnIcI2i(lnI-1,cI)
		C.refresh()
	def down(C):
		lnI,cI=C.i2lnIcI(C.curI)
		C.curI=C.lnIcI2i(lnI+1,cI)
		if C.curI>=len(C.buff)-1: C.curI=len(C.buff)-1
		C.refresh()

	# others
	
	#~ def enter(C): C.add('\n')
	def backspace(C):
		if C.curI==0: return
		C.left()
		C.buff.pop(C.curI)
		C.refresh()
	def delete(C):
		if C.curI>=len(C.buff)-1: return
		C.buff.pop(C.curI)
		C.refresh()
	# TODO
	def home(C):
		C.curI=C.lnIL[C.buff[C.curI]['lnI']]
		C.refresh()
	def end(C): pass
	def pageUp(C): pass
	def pageDown(C): pass
	
	
