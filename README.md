#ISO 

an emulated hardware enforced capability forth machine

```
opcodes:

cat: (a) (b) -- (a b)
qut: a -- (a)
unq: a -- a
nip: a b -- b
ovr: a b -- a b a
swp: a b -- b a
rot: a b c -- c a b
dup: a -- a a
cut: a b c -- b c
pop: a --
str: a b -- [b] <- a
add: a b -- a+b
sub: a b -- a-b
mul: a b -- a*b
div: a b -- a/b
eq0: a b c -- (c==0) ? a : b
hlt: -- halt machine
ptr: (a) -- a as a literaladdress
csh: a -- push capability of a
cop: -- pop capability

```
