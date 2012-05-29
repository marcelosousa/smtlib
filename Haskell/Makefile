CABAL-CONFIGURE-FLAGS 	:= --user
CABAL-BUILD-FLAGS     	:=
VERSION					:= 0.10.2

all : haskell doc

haskell :
	cabal install

doc :
	cabal haddock --executables --haddock-options=--odir=doc/haddock
  
dist:
	tar tfz imp-$(VERSION).tar.gz $(AG)

.PHONY : haskell dist
