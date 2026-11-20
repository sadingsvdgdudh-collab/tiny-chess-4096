import json as J,os,sys
from urllib.request import Request as Q,urlopen as U
from urllib.error import HTTPError
E=os.environ.get("MAKEFUN_ENV_URL","")
N=(33,31,18,14,-14,-18,-31,-33);G=(1,-1,16,-16,15,17,-15,-17)
L=(15,17,-15,-17);T=(1,-1,16,-16);X=L+T
V=[0,100,300,300,500,900,0]
S=tuple(s for s in range(128)if not s&0x88)
C=60000;O=0
PT=(0,2,6,12,20,40,0,0)
NT=(-4,0,4,8)
def po(p,d):
 try:
  with U(Q(E+p,J.dumps(d).encode(),{"content-type":"application/json"}))as r:return J.load(r)
 except HTTPError as e:print(p+":"+str(e.code),file=sys.stderr);raise
def uc(m):
 f,t,q=m;return chr(97+(f&7))+str((f>>4)+1)+chr(97+(t&7))+str((t>>4)+1)+"q"*bool(q)
def pf(w):
 w=w.split();b=bytearray(128)
 for i,r in enumerate(w[0].split("/")):
  f=0
  for c in r:
   if c<"A":f+=int(c)
   else:b[(7-i)*16+f]="_pnbrqk".index(c.lower())|8*(c>"Z");f+=1
 return b,w[1]=="b"
def at(b,s,y):
 for d in(15,17):
  x=s+(d if y else-d)
  if not x&0x88 and b[x]==1|(y<<3):return 1
 for d in N:
  x=s+d
  if not x&0x88 and b[x]==2|(y<<3):return 1
 for d in G:
  x=s+d
  if not x&0x88 and b[x]==6|(y<<3):return 1
 for d in X:
  t=s
  while 1:
   t+=d
   if t&0x88:break
   p=b[t]
   if p:
    if p>>3==y and((d in L and(p&7)in(3,5))or(d in T and(p&7)in(4,5))):return 1
    break
 return 0
def gn(b,u):
 mv=[];v=u^1;ks=-1;A=mv.append;fw=16-u*32
 for f in S:
  p=b[f]
  if p<1 or p>>3!=u:continue
  t=p&7
  if t==6:ks=f
  if t==1:
   r=f>>4;lr=6-u*5;sr=1+u*5;o=f+fw
   if not o&0x88 and not b[o]:
    A((f,o,5*(r==lr)));
    if r==sr and not b[o+fw]:A((f,o+fw,0))
   for d in(fw-1,fw+1):
    x=f+d
    if x&0x88:continue
    g=b[x]
    if g and g>>3==v:A((f,x,5*(r==lr)))
  elif t in(2,6):
   for d in(N,G)[t>2]:
    x=f+d
    if x&0x88:continue
    g=b[x]
    if not g or g>>3==v:A((f,x,0))
  else:
   for d in(L if t==3 else(T if t==4 else X)):
    x=f
    while 1:
     x+=d
     if x&0x88:break
     g=b[x]
     if not g:A((f,x,0))
     elif g>>3==v:A((f,x,0));break
     else:break
 return mv,ks
def mk(b,u,m):
 f,t,q=m;nb=bytearray(b);nb[f]=0;nb[t]=(q|(u<<3))if q else b[f];return nb
def ev(b,u):
 s=0
 for x in S:
  p=b[x]
  if not p:continue
  t=p&7;c=p>>3;r=x>>4;f=x&7;r2=r^c*7;v=V[t]
  if t==1:v+=PT[r2]
  if t==2:v+=NT[min(min(r2,7-r2),min(f,7-f))]
  if t==6:
   dw=16-c*32;pc=1|(c<<3)
   for df in(-1,0,1):
    sq=x+dw+df
    if sq&0x88 or b[sq]!=pc:v-=30
  s+=(v,-v)[c]
 return(s,-s)[u]
def qs(b,u,d,a,be):
 global O;O+=1
 if O>C or d<1:return ev(b,u)
 st=ev(b,u)
 if st>=be:return be
 if st>a:a=st
 ms,ks=gn(b,u)
 for m in ms:
  if not b[m[1]]and not m[2]:continue
  if st+V[b[m[1]]&7]+V[m[2]]+200<a:continue
  nb=mk(b,u,m);nk=m[1]if m[0]==ks else ks
  if at(nb,nk,u^1):continue
  sc=-qs(nb,u^1,d-1,-be,-a)
  if sc>st:st=sc
  if st>a:a=st
  if a>=be:break
 return st
def se(b,u,d,a,be):
 global O;O+=1
 if O>C:return ev(b,u)
 if d<1:return qs(b,u,3,a,be)
 ms,ks=gn(b,u);ms.sort(key=lambda m:V[b[m[1]]&7]*10-V[b[m[0]]&7],reverse=1)
 best=-30001;fo=0
 for m in ms:
  nb=mk(b,u,m);nk=m[1]if m[0]==ks else ks
  if at(nb,nk,u^1):continue
  fo=1;sc=-se(nb,u^1,d-1,-be,-a)
  if sc>best:best=sc
  if best>a:a=best
  if a>=be:break
 if not fo:return-30000-d if at(b,ks,u^1)else 0
 return best
def pk(b,u):
 global O;O=0
 ms,ks=gn(b,u);lg=[]
 for m in ms:
  nb=mk(b,u,m);nk=m[1]if m[0]==ks else ks
  if not at(nb,nk,u^1):lg.append(m)
 if len(lg)<2:return lg[0]if lg else 0
 bm=lg[0]
 for d in range(1,21):
  sd=[]
  for m in lg:
   nb=mk(b,u,m);sc=-se(nb,u^1,d-1,-30001,30001)
   sd.append((sc,m))
   if O>C:break
  if O>C or not sd:break
  sd.sort(reverse=1);lg=[x[1]for x in sd];bm=lg[0]
 return bm
if E:
 while 1:
  g=po("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   b,u=pf(g["fen"]);mv=pk(b,u)
   if not mv:break
   g=po("/move",{"game":g["game"],"move":uc(mv)})
  print(g["game"],g["outcome"],file=sys.stderr)
