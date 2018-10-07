/*
* @Author: Nandadeep Davuluru
* @Date:   2018-10-05 15:24:34
* @Last Modified by:   nandadeepd
* @Last Modified time: 2018-10-06 17:16:00
*/

import org.scalatest.FunSuite
import Machine0._

class TestMachine0 extends FunSuite {

	// (a) 1 + (3 - 2)
	test("Expr: 1 + (3 - 2) = 2") {
		assert(exec(List(Const(1), Const(3), Const(2), Divrem, Pop, Plus), 2) == 2)
	}

	// (b) (2 * -3) - (5 / 3)
	test("(2 * -3) - (5 / 3)") {
		assert(exec(List(Const(2), Const(-3), Times, Const(5), Const(3), Divrem, Pop, Subtract), 2) == -7)
	}

	// (c) (-2 / 3) * 3 + (-2 % 3)
	test("(-2 / 3) * 3 + (-2 % 3)") {
		assert(exec(List(Const(-2), Const(3), Divrem, Pop, Const(3), Times, Const(-2), Const(3), Divrem, Swap, Pop, Plus), 2) == -2)
	}



}