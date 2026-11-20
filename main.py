import json as J,os,sys
from urllib.request import Request as Q,urlopen as U
from urllib.error import HTTPError
E=os.environ.get("MAKEFUN_ENV_URL","")
N=(33,31,18,14,-14,-18,-31,-33);G=(1,-1,16,-16,15,17,-15,-17)
L=(15,17,-15,-17);T=(1,-1,16,-16);X=L+T
V=[0,100,300,300,500,900,0]
S=tuple(s for s in range(128)if not s&0x88)
C=18000;O=0
def po(p,d):
 try:
  with U(Q(E+p,J.dumps(d).encode(),{"content-type":"application/json"}))as r:return J.load(r)
 except HTTPError as e:print(p+":"+str(e.code),file=sys.stderr);raise
def uc(m):
 f,t,q=m;return chr(97+(f&7))+str((f>>4)+1)+chr(97+(t&7))+str((t>>4)+1)+("q"if q else"")
def pf(w):
 w=w.split();b=bytearray(128)
 for i,r in enumerate(w[0].split("/")):
  f=0
  for c in r:
   if c<"A":f+=int(c)
   else:b[(7-i)*16+f]="_pnbrqk".index(c.lower())|(8 if c>"Z"else 0);f+=1
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
 for d in L:
  t=s
  while 1:
   t+=d
   if t&0x88:break
   p=b[t]
   if p:
    if p>>3==y and(p&7)in(3,5):return 1
    break
 for d in T:
  t=s
  while 1:
   t+=d
   if t&0x88:break
   p=b[t]
   if p:
    if p>>3==y and(p&7)in(4,5):return 1
    break
 return 0
def gn(b,u):
 mv=[];v=u^1;ks=-1
 for f in S:
  p=b[f]
  if not p or p>>3!=u:continue
  t=p&7
  if t==6:ks=f
  if t==1:
   fw=16 if u==0 else-16;r=f>>4;lr=6 if u==0 else 1;sr=1 if u==0 else 6;o=f+fw
   if not o&0x88 and not b[o]:
    if r==lr:mv.append((f,o,5))
    else:
     mv.append((f,o,0))
     if r==sr and not b[o+fw]:mv.append((f,o+fw,0))
   for d in(fw-1,fw+1):
    x=f+d
    if x&0x88:continue
    g=b[x]
    if g and g>>3==v:
     if r==lr:mv.append((f,x,5))
     else:mv.append((f,x,0))
  elif t==2 or t==6:
   for d in(N if t==2 else G):
    x=f+d
    if x&0x88:continue
    g=b[x]
    if not g or g>>3==v:mv.append((f,x,0))
  else:
   for d in(L if t==3 else(T if t==4 else X)):
    x=f
    while 1:
     x+=d
     if x&0x88:break
     g=b[x]
     if not g:mv.append((f,x,0))
     elif g>>3==v:mv.append((f,x,0));break
     else:break
 return mv,ks
def mk(b,u,m):
 f,t,q=m;nb=bytearray(b);nb[f]=0;nb[t]=(q|(u<<3))if q else b[f];return nb
def ev(b,u):
 s=0
 for x in S:
  p=b[x]
  if not p:continue
  t=p&7;c=p>>3;r=x>>4;f=x&7;r2=r if c==0 else 7-r;v=V[t]
  if t==1:v+=(0,0,4,8,14,24,40,0)[r2]
  if t==2:v+=(min(f,7-f)+min(r2,7-r2))*4-12
  s+=v if c==0 else-v
 return s if u==0 else-s
def se(b,u,d,a,be):
 global O;O+=1
 if O>C or d<1:return ev(b,u)
 ms,ks=gn(b,u);ms.sort(key=lambda m:V[b[m[1]]&7]*10-V[b[m[0]]&7],reverse=True)
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
 if not lg:return
 if len(lg)==1:return lg[0]
 bm=lg[0]
 for d in range(1,21):
  sd=[]
  for m in lg:
   nb=mk(b,u,m);sc=-se(nb,u^1,d-1,-30001,30001)
   sd.append((sc,m))
   if O>C:break
  if O>C or not sd:break
  sd.sort(key=lambda x:-x[0]);lg=[x[1]for x in sd];bm=lg[0]
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
