# Makefile for CS558 Assignment 2
#

SC = scalac
TestFlag = -cp $(HOME)/scalatest-app_2.12-3.0.5.jar

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

sexpr:	SExprLibrary.class 

el0:	sexpr EL0.class
mach0: 	Machine0.class
int0: 	sexpr el0 Interp0.class
comp0: 	sexpr el0 mach0 Comp0.class

testsexpr: sexpr TestSExpr.scala
	$(SC) $(TestFlag) SExprLibrary.scala TestSExpr.scala

testel0: sexpr el0 TestEL0.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL0.scala TestEL0.scala

testmach0: mach0 TestMachine0.scala
	$(SC) $(TestFlag) Machine0.scala TestMachine0.scala

testint0: sexpr el0 int0 TestInterp0.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL0.scala \
           Interp0.scala TestInterp0.scala

testcomp0: sexpr el0 mach0 comp0 TestComp0.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL0.scala \
           Machine0.scala Comp0.scala TestComp0.scala

clean:	
	rm *.class
