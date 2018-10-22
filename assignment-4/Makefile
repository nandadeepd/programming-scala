# Makefile for CS558 Assignment 4
#

SC = scalac
TestFlag = -cp $(HOME)/scalatest-app_2.12-3.0.5.jar

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

sexpr:	SExprLibrary.class 

el1:	sexpr EL1.class
mach1: 	Machine1.class
comp1: 	sexpr el1 mach1 Comp1.class

testcomp1: sexpr el1 mach1 comp1 TestComp1.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL1.scala \
           Machine1.scala Comp1.scala TestComp1.scala

clean:	
	rm *.class
