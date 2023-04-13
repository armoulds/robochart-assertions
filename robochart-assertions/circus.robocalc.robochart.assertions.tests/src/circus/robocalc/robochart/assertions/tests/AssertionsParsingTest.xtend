/********************************************************************************
 * Copyright (c) 2019 University of York and others
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *   Alvaro Miyazawa - initial definition
 ********************************************************************************/

package circus.robocalc.robochart.assertions.tests

import circus.robocalc.robochart.assertions.assertions.RAPackage
import com.google.inject.Inject
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.extensions.InjectionExtension
import org.eclipse.xtext.testing.util.ParseHelper
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.^extension.ExtendWith

import circus.robocalc.robochart.assertions.assertions.AssertionsPackage

@ExtendWith(InjectionExtension)
@InjectWith(AssertionsInjectorProvider)
class AssertionsParsingTest {
	@Inject
	ParseHelper<RAPackage> parseHelper
	
	@Inject
	ValidationTestHelper validationHelper
	
	@Test
	def void loadCSPModel() {
		val result = parseHelper.parse('''
			csp PP csp-begin PP = SKIP csp-end
			assertion A1: PPs is not deadlock-free.
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void loadCSPModel1() {
		val result = parseHelper.parse('''
			csp P0 csp-begin P0 = SKIP csp-end
			assertion A1: P0 is not deadlock-free.
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void LabelTest() {
		val result = parseHelper.parse('''
			label l1 = true
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void LabelTest1() {
		val result = parseHelper.parse('''
			label l1 = (1 > 2)
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void FormulaTest() {
		val result = parseHelper.parse('''
			formula f1 = 1
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void ConstantTest() {
		val result = parseHelper.parse('''
			const c: core::int
			const d: core::boolean
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void ConstantsTest() {
		val result = parseHelper.parse('''
			constants cs1: dieSTM::cc set to 1
			
			constants cs2: dieSTM::d from set {1 to 10}
			
			constants cs3: DieM::CTRL::stm_ref0 from set {1,2}
			
			constants cs4: DieM::CTRL::stm_ref0 set to 1
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void RewardsTest() {
		val result = parseHelper.parse('''
				rewards r1 = 
					true : 1;
				endrewards
				rewards r2 = 
					true : 1;
				[e.in] (v > 1) : 1;
				endrewards
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	/////////////////////// Properties //////////////////////
	@Test
	def void ProbTest() {
		val result = parseHelper.parse('''
				prob property P0: true
				prob property P1: not true
				prob property P2: Prob=? of [Finally dieSTM::d > 1]
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void ProbTest1() {
		val result = parseHelper.parse('''
				prob property P0: not Exists [ Finally deadlock ]
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	/////////////////////// Expressions //////////////////////
	@Test
	def void ProbTestExp1() {
		val result = parseHelper.parse('''
				prob property P0: 1
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
		validationHelper.assertError(result, 
			AssertionsPackage.Literals.PROB_ASSERTION,
			'NotBooleanOrQueryInProbAssertion'
		)
	}
	
	@Test
	def void ProbTestExp2() {
		val result = parseHelper.parse('''
				prob property P0: 1 iff 2
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
		validationHelper.assertError(result, 
			AssertionsPackage.Literals.PIFF,
			'NotBooleanInIffExpr'
		)
	}
	
		@Test
	def void ProbTestExp20() {
		val result = parseHelper.parse('''
				prob property P0: 1 != 2 iff 2 == 1
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void ProbTestExp21() {
		val result = parseHelper.parse('''
				prob property P0: 1 /\ 2
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
		validationHelper.assertError(result, 
			AssertionsPackage.Literals.PAND,
			'NotBooleanInAndExpr'
		)
	}
	
	@Test
	def void ProbTestExp22() {
		val result = parseHelper.parse('''
				prob property P0: not 1
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
		validationHelper.assertError(result, 
			AssertionsPackage.Literals.PNOT,
			'NotBooleanInNotExpr'
		)
	}
	
	@Test
	def void ProbTestExp3() {
		val result = parseHelper.parse('''
				prob property P0: 1 == 2
				prob property P1: 1 != 2
				prob property P2: 1 <= 2
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void ProbTestExp_PathFormula_Finally_1() {
		val result = parseHelper.parse('''
				prob property P0: Finally false
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
		validationHelper.assertError(result, 
			AssertionsPackage.Literals.PROB_ASSERTION,
			'NotPathFormulaInProbAssertion'
		)
	}
	
	@Test
	def void ProbTestExp_PathFormula_Finally_2() {
		val result = parseHelper.parse('''
				prob property P0: Exists [ Finally false ]
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
		validationHelper.assertNoError(result, '')
	}
	
	@Test
	def void ProbTestExp_PathFormula_Finally_3() {
		val result = parseHelper.parse('''
				prob property P0: Exists [ Finally <=10 false ]
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
		validationHelper.assertNoError(result, '')
	}
	
	@Test
	def void ProbTestExp_PathFormula_Until_1() {
		val result = parseHelper.parse('''
				prob property P0: true Until false
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Expected errors: «errors.join(", ")»''')
	}
	
	@Test
	def void ProbTestExp_PathFormula_Until_2() {
		val result = parseHelper.parse('''
				prob property P0: Prob=? of [true Until false]
		''')
		Assertions.assertNotNull(result)
		val errors = result.eResource.errors
		Assertions.assertTrue(errors.isEmpty, '''Unexpected errors: «errors.join(", ")»''')
	}
	

}
