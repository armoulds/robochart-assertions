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

package circus.robocalc.robochart.assertions.validation

import circus.robocalc.robochart.Clock
import circus.robocalc.robochart.ConnectionNode
import circus.robocalc.robochart.ControllerDef
import circus.robocalc.robochart.ControllerRef
import circus.robocalc.robochart.Operation
import circus.robocalc.robochart.OperationDef
import circus.robocalc.robochart.RCModule
import circus.robocalc.robochart.StateMachine
import circus.robocalc.robochart.StateMachineDef
import circus.robocalc.robochart.Variable
import circus.robocalc.robochart.Event
import circus.robocalc.robochart.RoboticPlatformDef
import circus.robocalc.robochart.RoboticPlatformRef
import circus.robocalc.robochart.RoboticPlatform
import circus.robocalc.robochart.assertions.assertions.Assertion
import circus.robocalc.robochart.assertions.assertions.AssertionParameter
import circus.robocalc.robochart.assertions.assertions.AssertionsPackage
import circus.robocalc.robochart.assertions.assertions.BinaryAssertion
import circus.robocalc.robochart.assertions.assertions.CSPProcess
import circus.robocalc.robochart.assertions.assertions.UnaryAssertion
import java.util.HashSet
import org.eclipse.xtext.validation.Check
import circus.robocalc.robochart.assertions.assertions.Constants
import circus.robocalc.robochart.Variable
import circus.robocalc.robochart.VariableModifier
import circus.robocalc.robochart.StateMachineRef
import org.eclipse.xtext.EcoreUtil2
import circus.robocalc.robochart.assertions.assertions.*
import circus.robocalc.robochart.Type
import circus.robocalc.robochart.TypeRef
import circus.robocalc.robochart.PrimitiveType
import circus.robocalc.robochart.SeqType
import circus.robocalc.robochart.assertions.assertions.CSPProcess
import circus.robocalc.robochart.NamedElement
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference;
import java.util.List
import java.util.ArrayList
import circus.robocalc.robochart.Expression
import circus.robocalc.robochart.Iff
import circus.robocalc.robochart.Implies
import circus.robocalc.robochart.And
import circus.robocalc.robochart.Or
import circus.robocalc.robochart.Not
import circus.robocalc.robochart.Equals
import circus.robocalc.robochart.Different
import circus.robocalc.robochart.GreaterThan
import circus.robocalc.robochart.GreaterOrEqual
import circus.robocalc.robochart.LessThan
import circus.robocalc.robochart.LessOrEqual
import circus.robocalc.robochart.IfExpression
import circus.robocalc.robochart.ArrayExp
import circus.robocalc.robochart.BooleanExp
import circus.robocalc.robochart.IntegerExp
import circus.robocalc.robochart.FloatExp
import circus.robocalc.robochart.Plus
import circus.robocalc.robochart.Minus
import circus.robocalc.robochart.Div
import circus.robocalc.robochart.Mult

/**
 * This class contains custom validation rules. 
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation
 */
class AssertionsValidator extends AbstractAssertionsValidator {
	public static val INVALID_ASSERTION_ELEMENT = 'invalidAssertionElement'

	@Check
	def checkUnaryAssertion(UnaryAssertion a) {
		val el = a.element
		if (!(el instanceof Clock) && !(el instanceof CSPProcess) && !(el instanceof ConnectionNode) &&
			!(el instanceof Operation) && !(el instanceof RCModule)) {
			warning(
				'A unary assertion should refer to either a CSP process, state machine, clock,  operation, controller or module.',
				AssertionsPackage.Literals.UNARY_ASSERTION__ELEMENT, INVALID_ASSERTION_ELEMENT)
		}
	}

	@Check
	def checkBinaryAssertion(BinaryAssertion a) {
		var el = a.left
		if (!(el instanceof circus.robocalc.robochart.State) && !(el instanceof CSPProcess) &&
			!(el instanceof ConnectionNode) && !(el instanceof Operation) && !(el instanceof RCModule)) {
			warning(
				'A binary assertion\'s left element should refer to either a CSP process, state machine, state, operation, controller or module.',
				AssertionsPackage.Literals.BINARY_ASSERTION__LEFT, INVALID_ASSERTION_ELEMENT)
		}
		el = a.right
		if (!(el instanceof CSPProcess) && !(el instanceof ConnectionNode) && !(el instanceof Operation) &&
			!(el instanceof RCModule)) {
			warning(
				'A binary assertion\'s right element should refer to either a CSP process, state machine, operation, controller or module.',
				AssertionsPackage.Literals.BINARY_ASSERTION__RIGHT, INVALID_ASSERTION_ELEMENT)
		}
	}

	@Check
	def assertionConstantWellTyped(AssertionConstant a) {
		var t1 = a.variable.type
		t1 = t1.rewriteType(a.eResource)
		var t2 = a.value.typeFor
		t2 = t2.rewriteType(a.eResource)
		if (!typeCompatible(t2, t1) && t2 !== null) {
			val msg = '''Variable «a.variable.name» expects type «t1.printType», but «IF t1 === null»expression cannot be typed.«ELSE»expression has type «t2.printType»«ENDIF» '''
			error(
				msg,
				AssertionsPackage.Literals.ASSERTION_VALUE_SPECIFICATION__VALUE,
				'AssignmentTypeError'
			)
		}
	}

	@Check
	def checkAssertions(Assertion a) {
		if (a instanceof UnaryAssertion) {
			val el = a.element
			if (el instanceof StateMachineDef || el instanceof ControllerDef) {
				val consts = el.constantsRequired
				val sp_consts = a.parameters.map [ p |
					if(p instanceof AssertionConstant) p.variable.name else (p as AssertionParameter).variable.name
				]
				consts.removeAll(sp_consts)
				if (consts.size > 0) {
					warning(
						'The element ' + el.name + ' requires constants that are not specified in the assertion' +
							'''(«FOR c : consts SEPARATOR ','»«c»«ENDFOR»)''',
						AssertionsPackage.Literals.UNARY_ASSERTION__ELEMENT,
						'UnspecifiedRequiredVariables'
					)
				}
			}
		}
		if (a instanceof BinaryAssertion) {
			val el1 = a.left
			val el2 = a.right
			if (el1 instanceof StateMachineDef || el1 instanceof ControllerDef) {
				val consts = el1.constantsRequired
				val sp_consts = a.parameters.map [ p |
					if(p instanceof AssertionConstant) p.variable.name else (p as AssertionParameter).variable.name
				]
				consts.removeAll(sp_consts)
				if (consts.size > 0) {
					warning(
						'The element ' + el1.name + ' requires constants that are not specified in the assertion' +
							'''(«FOR c : consts SEPARATOR ','»«c»«ENDFOR»)''',
						AssertionsPackage.Literals.BINARY_ASSERTION__LEFT,
						'UnspecifiedRequiredVariables'
					)
				}
			}
			if (el2 instanceof StateMachineDef || el2 instanceof ControllerDef) {
				val consts = el2.constantsRequired
				val sp_consts = a.parameters.map [ p |
					if(p instanceof AssertionConstant) p.variable.name else (p as AssertionParameter).variable.name
				]
				consts.removeAll(sp_consts)
				if (consts.size > 0) {
					warning(
						'The element ' + el2.name + ' requires constants that are not specified in the assertion' +
							'''(«FOR c : consts SEPARATOR ','»«c»«ENDFOR»)''',
						AssertionsPackage.Literals.BINARY_ASSERTION__RIGHT,
						'UnspecifiedRequiredVariables'
					)
				}
			}
//			if (el1 instanceof OperationDef) {
//				val consts = el1.parameters.map[p| p.name]
//				val sp_consts = a.parameters.map [ p |
//					if (p instanceof AssertionConstant) p.variable.name
//					else (p as AssertionParameter).variable.name
//				]
//				consts.removeAll(sp_consts)
//				if (consts.size > 0) {
//					error(
//						'The operation ' + el1.name + ' requires parameters that are not specified in the assertion' +
//							'''(«FOR c : consts SEPARATOR ','»«c»«ENDFOR»)''',
//						AssertionsPackage.Literals.BINARY_ASSERTION__LEFT,
//						'UnspecifiedParameters'
//					)
//				}
//			}
//			if (el2 instanceof OperationDef) {
//				val consts = el2.parameters.map[p| p.name]
//				val sp_consts = a.parameters.map [ p |
//					if (p instanceof AssertionConstant) p.variable.name
//					else (p as AssertionParameter).variable.name;''
//				]
//				consts.removeAll(sp_consts)
//				if (consts.size > 0) {
//					error(
//						'The operation ' + el2.name + ' requires parameters that are not specified in the assertion' +
//							'''(«FOR c : consts SEPARATOR ','»«c»«ENDFOR»)''',
//						AssertionsPackage.Literals.BINARY_ASSERTION__RIGHT,
//						'UnspecifiedParameters'
//					)
//				}
//			}
		}
	}

	/*
	 * 	@Check
	 * 	def checkVarInConstants(Constants c) {
	 * 		for(cf: c.configs) {
	 * 			if(cf.^var instanceof Variable) {
	 * 				if(cf.^var.modifier !== VariableModifier::CONST) {
	 * 					error(
	 * 						'Only constant variables are allowed in a \'constants configuration\' ' + cf,
	 * 						AssertionsPackage.Literals.CONFIG__VAR,
	 * 						'VariableInConstantsError'
	 * 					)
	 * 				}
	 * 			}
	 * 		}
	 * 	}
	 */
/////////////////////////// Probabilistic assertions [START] ///////////////////////////////////////////////
	@Check
	def checkProbAssertion(ProbAssertion pa) {
		/*if(pa.inconfigs !== null && !pa.inconfigs.isEmpty) {
		 * 	checkInputConfigs(pa.inconfigs)
		 }*/
		 if(pa.form !== null && !isBooleanExpr(pa.form) && !isQueryExpr(pa.form)) {
		 	error(
				'A property to be checked must be boolean or a query',
				AssertionsPackage.Literals.PROB_ASSERTION__FORM,
				'NotBooleanOrQueryInProbAssertion'
			)
		 }
		 
		 // Not path formulas
		 if(pa.form !== null && isPathFormulaExpr(pa.form)) {
		 	error(
				'A property to be checked must not be a path formula',
				AssertionsPackage.Literals.PROB_ASSERTION__FORM,
				'NotPathFormulaInProbAssertion'
			)
		 }
	}

	// return a QNRef's object class (depending on its last tail)
	def EObject getQNRefObj(QNRef ref) {
		if (ref instanceof NameRef) {
			ref.name
		} else if (ref instanceof QualifiedNameToElement) {
			ref.tail
		}
	}

	// return a QNRef's object class (depending on the parent of the last tail)
	def EObject getQNRefObj1(QNRef ref) {
		if (ref instanceof NameRef) {
			ref.name
		} else if (ref instanceof QualifiedNameToElement) {
			if (ref.ref instanceof NameRef) {
				(ref.ref as NameRef).name
			} else if (ref.ref instanceof QualifiedNameToElement) {
				(ref.ref as QualifiedNameToElement).tail
			}
		}
	}
	
	def String printQNRef(QNRef ref) {
		if (ref instanceof NameRef) {
			return ref.name.name
		} else if (ref instanceof QualifiedNameToElement) {
			return printQNRef(ref.ref) + "::" + ref.tail.name
		}
	}

	@Check
	def checkStateInSubstate(circus.robocalc.robochart.assertions.assertions.pStmInStateBoolExpr c) {
		if (c !== null) {
			if (getQNRefObj(c.parent) instanceof StateMachineDef || getQNRefObj(c.parent) instanceof StateMachineRef ||
				getQNRefObj(c.parent) instanceof circus.robocalc.robochart.State) {
				if (getQNRefObj(c.parent) instanceof circus.robocalc.robochart.State) {
					if (!isCompositeState(getQNRefObj(c.parent) as circus.robocalc.robochart.State))
						error(
							'Only a composite state can be checked if it is in a substate',
							AssertionsPackage.Literals.PSTM_IN_STATE_BOOL_EXPR__PARENT,
							'NotACompositeStateError'
						)
				}
			} else {
				error(
					'Only a state machine or a composite state can be checked if it is in a substate',
					AssertionsPackage.Literals.PSTM_IN_STATE_BOOL_EXPR__PARENT,
					'NotAStateError'
				)
			}
		}
	}

	@Check
	def checkStateInDirectSubstate(circus.robocalc.robochart.assertions.assertions.pStmInStateBoolExpr c) {
		if (c !== null) {
			if (getQNRefObj(c.parent) instanceof StateMachineDef ||
				getQNRefObj(c.parent) instanceof circus.robocalc.robochart.State) {
				if (getQNRefObj(c.child).eContainer !== getQNRefObj(c.parent)) {
					error(
						'The state ' + getQNRefObj(c.child).name + "is not a substate of " + getQNRefObj(c.parent).name,
						AssertionsPackage.Literals.PSTM_IN_STATE_BOOL_EXPR__PARENT,
						'NotAParentChildRelError'
					)
				}
			} else if (getQNRefObj(c.parent) instanceof StateMachineRef) {
				val def = (getQNRefObj(c.parent) as StateMachineRef).ref
				if (getQNRefObj(c.child).eContainer !== def) {
					error(
						'The state ' + getQNRefObj(c.child).name + "is not a substate of " + getQNRefObj(c.parent).name,
						AssertionsPackage.Literals.PSTM_IN_STATE_BOOL_EXPR__PARENT,
						'NotAParentChildRelError'
					)
				}

//				// Get this object's root element, should be RCPackage
//				// But StateMachineRef may be in another RCPackage
//				val rootElement = EcoreUtil2.getRootContainer(getQNRefObj(c.child).eContainer);
//				val resources = rootElement.eResource.resourceSet.resources
//				
//				val candidates = EcoreUtil2.getAllContentsOfType(rootElement, StateMachineRef);
//				// traverse all resources to find
//				for(r: resources) {
//					r.getAllContents.filter[n|n instanceof StateMachineRef].forEach[s|candidates.add(s as StateMachineRef)]
//				}
//				
//				if(!candidates.contains(getQNRefObj(c.parent))) {
//					error(
//							'The state ' + getQNRefObj(c.child).name + " is not a substate of " + getQNRefObj(c.parent).name,
//							AssertionsPackage.Literals.PSTM_IN_STATE_BOOL_EXPR__PARENT,
//							'NotAParentChildRelError'
//						)
//				}
			}
		}
	}

	@Check
	def checkExprInLabel(Label l) {
		if (l !== null) {
			if (l.expr !== null) {
				if (!isBooleanExpr(l.expr)) {
					error(
						'The expression in a label definition ' + l.name + " should be a boolean expression ",
						AssertionsPackage.Literals.LABEL__EXPR,
						'labelExprNotBooleanError'
					)
				}
			}
		}
	}

	@Check
	def checkNameInLabel(Label l) {
		if (l !== null) {
			if (l.name !== null) {
				if (l.name.equals("init") || l.name.equals("deadlock")) {
					error(
						'The label name ' + l.name +
							' is predefined built-in and couldn\'t not be used to define label',
						AssertionsPackage.Literals.LABEL__NAME,
						'labelNameBuiltInError'
					)
				}
			}
		}
	}

	
//	@Check
//	def checkExprInFormula(Formula f) {
//		if(f !== null) {
//			if(f.expr !== null) {
//				if(isBooleanExpr(f.expr)) {
//					error(
//							'The expression in a formula definition ' + f.name + " shouldn't be a boolean expression ",
//							AssertionsPackage.Literals.FORMULA__EXPR,
//							'formulaExprIsBooleanError'
//						)
//				}
//			}
//		}
//	}
	
	/*@Check
	def checkLabelFormula(LabelFormula f) {
		if(f !== null) {
			val name = f.name.replace('"', "").trim
			if(!name.equals("init") && !name.equals("deadlock")) {
				val rootElement = EcoreUtil2.getRootContainer(f.eContainer);
				val candidates = EcoreUtil2.getAllContentsOfType(rootElement, Label);
				var found = false
				for(c: candidates) {
					if(c.name.equals(name)) {
						found = true;
					}
				}
				if(!found) {
					error(
					'The label name ' + f.name + ' doesn\'t exist. It could be built-in \"init\" or \"deadlock\", or defined',
						AssertionsPackage.Literals.LABEL_FORMULA__NAME,
						'labelNameNotExistentError'
					)
				}
			}
		}
	}*/
//	
//	@Check
//	def checkExprInSetExpr(SetExpr e) {
//		if(e !== null) {
//			if(e.init !== null && e.final !== null && e.step !== null) {
//				if(isLiteralIntNumExpr(e.init) && isLiteralIntNumExpr(e.final) && isLiteralIntNumExpr(e.step)) {
//				} else {
//					error(
//					'The "from set init:final:step" requires all three expressions to be literal numbers or +/-/* of them',
//						AssertionsPackage.Literals.SET_EXPR__INIT,
//						'notAllExprsLiteralNumError'
//					)
//				}
//				
//			} else if (e.init === null && e.final === null && e.step === null) {
//				if(e.values !== null) {
//					for(v:e.values) {
//						if(!isLiteralIntNumExpr(v)) {
//							error(
//							'The "from set {a,...}" requires all enumeration expressions to be literal numbers or +/-/* of them',
//								AssertionsPackage.Literals.SET_EXPR__VALUES,
//								'notAllExprsLiteralNumError'
//							)
//						}
//					}
//				}
//			} else {
//				error(
//					'The "from set init:final:step" needs all three expressions to be provided',
//						AssertionsPackage.Literals.SET_EXPR__INIT,
//						'notAllExprsProvidedError'
//					)
//			}
//
//		}
//	}
//	
	@Check
	def checkSimMethod(CiMethod e) {
		if(e !== null) {
			var num = 0
			if(e.w !== null) {
				num++
			}
			
			if(e.alpha !== null) {
				num++
			}
			
			if(e.n !== null) {
				num++
			}
			
			if(num !== 2) {
				error(
					'Exactly two of three parameters [w, alpha and n] should be provided for CI or ACI, but ' + num + ' is provided',
						AssertionsPackage.Literals.CI_METHOD__W,
						'notExactlyTwoParasProvidedError'
					)
			}
		}
	}
	
	@Check
	def checkAPMCMethod(APMCMethod e) {
		if(e !== null) {
			var num = 0
			if(e.epsilon !== null) {
				num++
			}
			
			if(e.delta !== null) {
				num++
			}
			
			if(e.n !== null) {
				num++
			}
			
			if(num !== 2) {
				error(
					'Exactly two of three parameters [epsilon, delta and n] should be provided for APMC, but ' + num + ' is provided',
						AssertionsPackage.Literals.APMC_METHOD__EPSILON,
						'notExactlyTwoParasProvidedError'
					)
			}
		}
	}
	
	@Check
	def checkSPRTMethod(PFormula e) {
		if(e !== null) {
			if(e.mp !== null) {
				if(e.mp.sim !== null) {
					if(e.mp.sim instanceof SPRTSimMethod) {
						if(e.bp === null) {
							error(
								'SPRT method is only available for bounded properties',
									AssertionsPackage.Literals.PFORMULA__BP,
									'sprtNotValidForUnboundedPropError'
								)
						}
					}
				}
			}
		}
	}
	
	@Check
	def checkSPRTMethod(RFormula e) {
		if(e !== null) {
			if(e.mr !== null) {
				if(e.mr.sim !== null) {
					if(e.mr.sim instanceof SPRTSimMethod) {
						if(e.br === null) {
							error(
								'SPRT method is only available for bounded properties',
									AssertionsPackage.Literals.RFORMULA__BR,
									'sprtNotValidForUnboundedPropError'
								)
						}
					}
				}
			}
		}
	}
//	
////	@Check
////	def checkFormulaRef(FormulaRefExpr e) {
////		if(e !== null) {
////			if(e.eContainer !== null) {
////				val rootElement = EcoreUtil2.getRootContainer(e.eContainer);
////				val candidates = EcoreUtil2.getAllContentsOfType(rootElement, Formula);
////				if(candidates.filter[c|c.name.equals(e.fname)].isEmpty) {
////					error(
////							'The formula name ' + e.fname + " hasn't been defined ",
////							AssertionsPackage.Literals.FORMULA_REF_EXPR__FNAME,
////							'notDefinedFormulaNameError'
////						)
////				}
////			}
////		}
////	}
//		
	def boolean isCompositeState(circus.robocalc.robochart.State s) {
		if (s !== null) {
			if (!(s.getNodes().isEmpty()) && !(s.getTransitions().isEmpty())) {
				return true;
			}
		}

		return false;
	}

	def boolean isPathFormulaExpr(pExpression e) {
		if (e instanceof pPathFormula) {
			return true
		} else if (e instanceof UntilFormula) {
			return true
		} else if (e instanceof NextFormula) {
			return true
		} else if (e instanceof FinalFormula) {
			return true
		} else if (e instanceof GlobalFormula) {
			return true
		} else if (e instanceof WeakFormula) {
			return true
		} else if (e instanceof RelFormula) {
			return true
		} else if (e instanceof ReachAwardFormula) {
			return true
		} else if (e instanceof CumulAwardFormula) {
			return true
		} else if (e instanceof TotalAwardFormula) {
			return true
		}
		
		return false
	}
	
	def boolean isBooleanExpr(pExpression e) {
		if (e instanceof pIff) {
			return true;
		} else if (e instanceof pImplies) {
			return true;
		} else if (e instanceof pAnd) {
			return true;
		} else if (e instanceof pOr) {
			return true;
		} else if (e instanceof pNot) {
			return true;
		} else if (e instanceof pEquals) {
			return true;
		} else if (e instanceof pDifferent) {
			return true;
		} else if (e instanceof pGreaterThan) {
			return true;
		} else if (e instanceof pGreaterOrEqual) {
			return true;
		} else if (e instanceof pLessThan) {
			return true;
		} else if (e instanceof pLessOrEqual) {
			return true;
		} else if (e instanceof pIfExpression) {
			return isBooleanExpr(e.ifexp) && isBooleanExpr(e.elseexp)
		} else if (e instanceof pArrayExp) {
			return isBooleanExpr(e.value)
		} else if (e instanceof pBooleanExp) {
			return true
		} else if (e instanceof pRefExpr) {
			if(getQNRefObj(e.ref) instanceof Variable) {
				var v = (getQNRefObj(e.ref) as Variable)
				return isBooleanType(v.type)
			} 
			return false
		} else if (e instanceof pStmInStateBoolExpr) {
			return true
		} else if (e instanceof pFormulaRefExpr) {
			return isBooleanExpr(e.fname.expr)
		} else if (e instanceof pRefFPara) {
			return true
		} else if (e instanceof pFCallExpr) {
			return isBooleanExpr(e.pfname.expr)
		} else if (e instanceof pParExp) {
			return isBooleanExpr(e.exp)
		} else if (e instanceof pStateFormula) {
			return isBooleanExpr(e.stform)
		} else if(e instanceof OpPFormula) {
			var form = e as OpPFormula
			if(form.pform.bp !== null) {
				return true
			} else if (form.pform.qp !== null) {
				return false
			}
			return true
		} else if(e instanceof OpRFormula) {
			var form = e as OpRFormula
			if(form.rform.br !== null) {
				return true
			} else if (form.rform.qr !== null) {
				return false
			}
			return true
		}
		else if(e instanceof OpAFormula) {
			return true
		}
		else if(e instanceof OpEFormula) {
			return true
		}
		else if(e instanceof LabelFormula) {
			return true
		}
		else if (e instanceof pPathFormula) {
			return true
		} else if (e instanceof UntilFormula) {
			return true
		} else if (e instanceof pRPathFormula) {
			return true
		} else if (e instanceof ReachAwardFormula) {
			return true
		} else if (e instanceof CumulAwardFormula) {
			return true
		} else if (e instanceof TotalAwardFormula) {
			return true
		} else if (e instanceof pVariableRef) {
			if((e.ref.type) instanceof PBOOL) {
				return true;
			} 
			return false
		}

		return false;
	}
	
	def boolean isNumExpr(pExpression e) {
		if (e instanceof pPlus) {
			return true;
		} else if (e instanceof pMinus) {
			return true;
		} else if (e instanceof pMult) {
			return true;
		} else if (e instanceof pDiv) {
			return true;
		} else if (e instanceof pNeg) {
			return true;
		} else if (e instanceof pModulus) {
			return true;
		} else if (e instanceof pIfExpression) {
			return isNumExpr(e.ifexp) && isNumExpr(e.elseexp)
		} else if (e instanceof pArrayExp) {
			return isNumExpr(e.value)
		} else if (e instanceof pRefExpr) {
			if(getQNRefObj(e.ref) instanceof Variable) {
				var v = (getQNRefObj(e.ref) as Variable)
				return isIntegerType(v.type) || isRealType(v.type) ;
			} 
			return false
		} else if (e instanceof pIntegerExp) {
			return true
		} else if (e instanceof pFloatExp) {
			return true
		} else if (e instanceof pParExp) {
			return isNumExpr(e.exp)
		} else if (e instanceof pFormulaRefExpr) {
			return isNumExpr(e.fname.expr)
		} else if (e instanceof pRefFPara) {
			return true
		} else if (e instanceof pFCallExpr) {
			return isNumExpr(e.pfname.expr)
		} else if (e instanceof pStateFormula) {
			return isNumExpr(e.stform)
		}
		else if(e instanceof OpPFormula) {
			if(e.pform.bp !== null) {
				return false
			} else if (e.pform.qp !== null) {
				return true
			}
		} else if(e instanceof OpRFormula) {
			if(e.rform.br !== null) {
				return false
			} else if (e.rform.qr !== null) {
				return true
			}
		} else if (e instanceof pVariableRef) {
			if((e.ref.type) instanceof PRange) {
				return true;
			} 
			return false
		}

		return false;
	}
	
	// additionally allow +, - and *
	def boolean isLiteralNumExpr(pExpression e) {
		if (e instanceof pIntegerExp) {
			return true
		} if (e instanceof pFloatExp) {
			return true
		} else if (e instanceof pPlus) {
			return isLiteralNumExpr(e.left) && isLiteralNumExpr(e.right)
		} else if (e instanceof pMinus) {
			return isLiteralNumExpr(e.left) && isLiteralNumExpr(e.right)
		} else if (e instanceof pDiv) {
			return false
		} else if (e instanceof pMult) {
			return isLiteralNumExpr(e.left) && isLiteralNumExpr(e.right)
		} 
		
		return false;
	}
	
	// additionally allow +, - and *
	def boolean isLiteralIntNumExpr(pExpression e) {
		if (e instanceof pIntegerExp) {
			return true
		} 
		
		return false;
	}
	
	def boolean isQueryExpr(pExpression e) {
		if (e instanceof pStateFormula) {
			return isQueryExpr(e.stform);
		} else if(e instanceof OpPFormula) {
			if(e.pform.bp !== null) {
				return false
			} else if (e.pform.qp !== null) {
				return true
			}
		} else if(e instanceof OpRFormula) {
			if(e.rform.br !== null) {
				return false
			} else if (e.rform.qr !== null) {
				return true
			}
		}

		return false;
	}
//
//    // TODO: need to check variable names in configs in a "constants" or "with constants" won't conflict
//
//    // TODO: need to check constant setting name in ProbFormula is defined 
//
//	// Check if an Event referred by a QualifiedNameToElement is an output event from the robotic platform
//	def isOutputEventFromRP(Event e, EReference ref) { 
//        // 1.1 it must be an event carrying message 
//        if(e.type === null) {
//            /* System.out.println("[isOutputEventFromRP] Event : " + e + 
//                "\n\t\tType: null" + 
//                "\n\t\tRef: " + ref)
//            */
//			error(
//				'Only *inputs events* are allowed in a \'constants configuration\' ',
//				ref,
//				'NotInputEventInConstantsError'
//			)
//        } else {
//            /* System.out.println("[isOutputEventFromRP] Event : " + e.toString() + 
//                "\n\t\tType: " + e.type + 
//                "\n\t\tRef: " + ref.toString())
//            */
//            // 1.2 the parent of this event shall be a RoboticPlatform 
//            val parent = e.eContainer
//            // System.out.println("Parent: " + parent.toString());
//            if ((parent instanceof RoboticPlatformDef) || (parent instanceof RoboticPlatformDef)) {
//                // 1.3 find out the root element of this RP, it should be a RCPackage 
//			    val rootElement = EcoreUtil2.getRootContainer(parent);
//                // System.out.println("rootElement: " + rootElement.toString());
//                // 1.4 find out the RCModule in the RCPackage 
//			    val candidates = EcoreUtil2.getAllContentsOfType(rootElement, RCModule);
//                // System.out.println("candidates: " + candidates.toString());
//                // 1.5 it should be not empty, but if it is empty, maybe this is not the correct way to find the Module 
//                if (candidates.isEmpty) {
//			        warning(
//				            'Cannot find a Module in the root of ' + parent.name,
//				            ref,
//				            'NotInputEventInConstantsError'
//                        )
//                    } else {
//                        // 1.6 we use the RP name (def or ref)  to check if the event is an output from RP or not.
//                        val event = e 
//                        val mod = (candidates.get(0) as RCModule)
//                        // System.out.println("Module: " + mod.toString());
//                        var rp_name = parent.name
//                        // System.out.println("rp_name: " + rp_name);
//                        // If there is a RoboticPlatformRef in the module, then get its name
//                        if (!mod.nodes.filter[n|n instanceof RoboticPlatformRef].isEmpty) {
//                            rp_name = (mod.nodes.filter[n|n instanceof RoboticPlatformRef].get(0) as RoboticPlatformRef).name
//                        } 
//                        // System.out.println("rp_name: " + rp_name);
//                        val rp_name1 = rp_name
//                        // System.out.println("mod.connections: " + mod.connections.toString());
//                        if (mod.connections.filter[c|c.from.name == rp_name1 && c.efrom == event].isEmpty) {
//				            error(
//				                'Only *outputs events* of robotic platforms are allowed in a \'constants configuration\' ',
//				                ref,
//				                'NotInputEventInConstantsError'
//				            )
//                        }
//                   }
//            } else {
//			    error(
//			        'Only events from a *robotic platform* are allowed in a \'constants configuration\' ',
//				    ref,
//				    'NotEventFromRPConstantsError'
//				)
//            } 
//        }
//    }
//
//	@Check
//	def checkVarInConfig(Config cf) {
//		/*if(getQNRefObj(cf.^var) instanceof Variable) {
//			if(cf.^var.modifier !== VariableModifier::CONST) {
//				error(
//					'Only constant variables are allowed in a \'constants configuration\' ' + cf,
//					AssertionsPackage.Literals.CONFIG__VAR,
//					'VariableInConstantsError'
//				)
//			}
//		} else*/ 
//        // 1. if the element to be configured is an event, 
//        if (getQNRefObj(cf.^var) instanceof Event) {
//            isOutputEventFromRP((getQNRefObj(cf.^var) as Event), AssertionsPackage.Literals.CONFIG__VAR)
//
//            /* if ((getQNRefObj1(cf.^var) instanceof RoboticPlatformDef) || (getQNRefObj1(cf.^var) instanceof RoboticPlatformDef)) {
//                // TODO: check if this is an output from the robotic platform  
//			    warning(
//			        'Only inputs events from robotic platforms are allowed in a \'constants configuration\' ' + getQNRefObj1(cf.^var),
//			        AssertionsPackage.Literals.CONFIG__VAR,
//			        'NotInputEventInConstantsError'
//                    )
//            } else {
//			    warning(
//			        'Only inputs events from robotic platforms are allowed in a \'constants configuration\' ' + getQNRefObj1(cf.^var),
//			        AssertionsPackage.Literals.CONFIG__VAR,
//			        'NotInputEventInConstantsError'
//                    )
//            }
//
//			val rootElement = EcoreUtil2.getRootContainer(getQNRefObj1(cf.^var));
//			val candidates_rpd = EcoreUtil2.getAllContentsOfType(rootElement, RoboticPlatformDef);
//			val candidates_rpr = EcoreUtil2.getAllContentsOfType(rootElement, RoboticPlatformRef);
//
//            if(candidates_rpd.filter[c|c.name.equals(getQNRefObj1(cf.^var).name)].isEmpty && 
//               candidates_rpr.filter[c|c.name.equals(getQNRefObj1(cf.^var).name)].isEmpty) 
//			    error(
//			        'Only inputs events from robotic platforms are allowed in a \'constants configuration\' ' + getQNRefObj1(cf.^var),
//			        AssertionsPackage.Literals.CONFIG__VAR,
//			        'NotInputEventInConstantsError'
//			    )
//            */
//	    }
//	}
//
//	/*@Check
//	def checkRPContext(RPContext c) {
//		checkInputConfigs(c.inconfigs)
//	}
//	
//	def checkInputConfigs(List<InputConfig> inconfigs) {
//        // System.out.println("checkRPContext")
//        // 1. qualified name should be an output event of the robotic platform 
//        // 2. Events should be distinct
//        var List<Event> events = new ArrayList<Event>
//        if(inconfigs.isEmpty) { 
//            error(
//			    'Empty input configurations',
//			    AssertionsPackage.Literals.RP_CONTEXT__INCONFIGS,
//			    'emptyInputConfigError'
//			)
//        } else {
//		    for(cf: inconfigs) {
//                if (getQNRefObj(cf.^var) instanceof Event) {
//                    val Event e = (getQNRefObj(cf.^var) as Event)
//                    // System.out.println("Event: " + e.name)
//                    if(events.contains(e)) {
//                        // System.out.println("Duplicate event: " + e.name)
//		    	        error(
//		    	            'An event [' + e.name + '] appears more than once in \'input configurations\' ',
//		    	            AssertionsPackage.Literals.INPUT_CONFIG__VAR,
//		    	            'moreThanOnceEventsInInputConfigError'
//		    	        )
//                        return
//                    } else {
//                        isOutputEventFromRP(e, AssertionsPackage.Literals.INPUT_CONFIG__VAR)
//                        events.add(e)
//                    }
//                } else {
//		    	    error(
//		    	        'Only *events* from robotic platforms are allowed in an \'input configuration\' ',
//		    	        AssertionsPackage.Literals.INPUT_CONFIG__VAR,
//		    	        'NotInputEventInInputConfigError'
//		    	    )
//                    return
//                }
//		    }
//
//            // 3. Events should cover all output events from RP
//            val e = (getQNRefObj(inconfigs.get(0).^var) as Event)
//            // 3.1 the parent of this event shall be a RoboticPlatform 
//            val parent = e.eContainer
//            // System.out.println("Parent: " + parent.toString());
//            if ((parent instanceof RoboticPlatformDef) || (parent instanceof RoboticPlatformDef)) {
//                // 3.2 find out the root element of this RP, it should be a RCPackage 
//			    val rootElement = EcoreUtil2.getRootContainer(parent);
//                // System.out.println("rootElement: " + rootElement.toString());
//                // 3.3 find out the RCModule in the RCPackage 
//			    val candidates = EcoreUtil2.getAllContentsOfType(rootElement, RCModule);
//                // System.out.println("candidates: " + candidates.toString());
//                // 3.4 it should be not empty, but if it is empty, maybe this is not the correct way to find the Module 
//                if (candidates.isEmpty) {
//                } else {
//                    // 3.5 we use the RP name (def or ref)  to check if the event is an output from RP or not.
//                    val event = e 
//                    val mod = (candidates.get(0) as RCModule)
//                    // System.out.println("Module: " + mod.toString());
//                    var rp_name = parent.name
//                    // System.out.println("rp_name: " + rp_name);
//                    // If there is a RoboticPlatformRef in the module, then get its name
//                    if (!mod.nodes.filter[n|n instanceof RoboticPlatformRef].isEmpty) {
//                        rp_name = (mod.nodes.filter[n|n instanceof RoboticPlatformRef].get(0) as RoboticPlatformRef).name
//                    } 
//                    // System.out.println("rp_name: " + rp_name);
//                    val rp_name1 = rp_name
//                    // System.out.println("mod.connections: " + mod.connections.toString());
//                    val conns = mod.connections.filter[cc|(cc.from.name == rp_name1) && (cc.efrom.type != null)]
//                    if (conns.size !== events.size) {
//				        error(
//				            '*Not all output events* of robotic platforms are configured: [' + events.size + '\\' + conns.size + ']',
//				            AssertionsPackage.Literals.RP_CONTEXT__INCONFIGS,
//				            'NotAllEventsConfiguredError'
//				        )
//                        return 
//                    }
//                }
//            }
//        }
//	}
//
//	@Check
//	def checkMapProb(MapProb mp) {
//        var sum = 0.0 
//		for(cf: mp.probs) {
//            if(cf.prob > 1.0 || cf.prob < 0) {
//                error(
//                    'Probability must be between 0 and 1',
//                    AssertionsPackage.Literals.VALUE_PROB__PROB,
//                    "probNotBetweenZeroAndOneError"
//                    ) 
//            } else {
//                sum = sum + cf.prob
//                // System.out.println("Sum: " + sum);
//            }
//		}
//
//        // sum should be 1
//        if (sum > 1.0 + 1e-6 || sum < 1.0 - 1e-6) {
//            error(
//                'Probabilities must add up to 1',
//                AssertionsPackage.Literals.MAP_PROB__PROBS,
//                "probsNotSumToOneError"
//                ) 
//        }
//	}
//
//	@Check
//	def checkRangeProb(RangeProb rp) {
//        if(rp.start > rp.end) {
//            error(
//                'The start integer should be less or equal than the end integer',
//                AssertionsPackage.Literals.RANGE_PROB__START,
//                "startLargerThanEndError"
//                 ) 
//        }
//	}*/
//
    def boolean isIntegerType (Type ct) {
        if(ct instanceof TypeRef) {
        	val t = ct as TypeRef
        	if(t.ref instanceof PrimitiveType) {
        		if(t.ref.name.equals("nat")|| t.ref.name.equals("int")) {
        			return true
        		}
        	}
        }
        return false
    }
    
    def boolean isRealType (Type ct) {
        if(ct instanceof TypeRef) {
        	val t = ct as TypeRef
        	if(t.ref instanceof PrimitiveType) {
        		if(t.ref.name.equals("real")) {
        			return true
        		}
        	}
        }
        return false
    }
    
    def boolean isBooleanType (Type ct) {
        if(ct instanceof TypeRef) {
        	val t = ct as TypeRef
        	if(t.ref instanceof PrimitiveType) {
        		if(t.ref.name.equals("boolean")) {
        			return true
        		}
        	}
        } else if(ct instanceof SeqType) {
        	return isBooleanType(ct.domain)
        }
        
        return false
    }
//
//    def boolean isExprMatchedWithEventType(Expr e, Type ct) {
//        if(ct instanceof TypeRef) {
//        	val t = ct as TypeRef
//        	if(t.ref instanceof PrimitiveType) {
//        		if(t.ref.name.equals("nat") || t.ref.name.equals("int")) {
//                    if(isLiteralIntNumExpr(e)) {
//                        return true
//                    }
//                } else if(t.ref.name.equals("boolean")) {
//                    if(isBooleanExpr(e)) {
//                        return true
//                    }
//                } else if(t.ref.name.equals("real")) {
//                    if(e instanceof IntExpr || e instanceof NegNumExpr || e instanceof FloatExpr) {
//                        return true
//                    }
//        	    }
//        	}
//        }
//        return false
//    }
//
///*	@Check
//	def checkInputConfig(InputConfig ic) {
//        // Get event's datatype
//        if (getQNRefObj(ic.^var) instanceof Event) {
//            val Event e = (getQNRefObj(ic.^var) as Event)
//            if (e.type !== null) {
//                if (ic.range !== null) {
//                    // type should be int or nat 
//                    if(!isIntegerType(e.type)) {
//                        error (
//                                'The event type is not an integer [' + e.type + ']. \nIt must be nat or int in order to use this range expression.',
//                                AssertionsPackage.Literals.INPUT_CONFIG__RANGE,
//                                'notIntegerTypeError'
//                              )
//                        return
//                    }
//                } else if (ic.map !== null) {
//                    for(prob: ic.map.probs) {
//                        if(!isExprMatchedWithEventType(prob.value, e.type)) {
//                            error (
//                                'The expression [' + prob.value + '] does not match with the event type [' + e.type + ']',
//                                AssertionsPackage.Literals.INPUT_CONFIG__RANGE,
//                                'notIntegerTypeError'
//                            )
//                            return 
//                        }
//                    }  
//                }
//            }
//        }    
//    }*/
//    
//    @Check
//    def checkPAssignment(PAssignment pa) {
//    	if(pa !== null) {
//    		if (! (getQNRefObj(pa.^var) instanceof Variable)) {
//                    error (
//                        'Only assignment to a *variable* is allowed in an operation, but it is ' + getQNRefObj(pa.^var),
//                        AssertionsPackage.Literals.PASSIGNMENT__VAR,
//                        'notAssignmentToVariableInOperation'
//                    )
//            }
//    	}
//    }
//    
//    @Check
//    def checkPOperation(POperation op) {
//    	if(op.assigns !== null) {
//    		for(assign: op.assigns) {
//    			
//    		}
//    	}
//    	
//    	return
//    }
//    
//    @Check
//    def checkArrayExpr(ArrayExpr e) {
//    	val ref = getQNRefObj(e.ref)
//    	if(! (ref instanceof Variable)) {
//    		error (
//                    'The reference in an array expression must be a variable' + ref.toString,
//                    AssertionsPackage.Literals.ARRAY_EXPR__REF,
//                    'NotRef2VariableInArrayExpr'
//                )
//            return
//    	} 
//    	
//		val t = (ref as Variable).type
//		if(!(t instanceof SeqType)) {
//    		error (
//                    'The *variable* in an array expression must be a SeqType, but it is ' + t.toString,
//                    AssertionsPackage.Literals.ARRAY_EXPR__REF,
//                    'NotSeqTypeInArrayExpr'
//                )
//            return
//    	} 
//    	
//    	for(p: e.parameters) {
//    		if(! isLiteralIntNumExpr(p)) {
//    			error (
//                    'The indices in an array expression must be an integer, but it is ' + p.toString,
//                    AssertionsPackage.Literals.ARRAY_EXPR__PARAMETERS,
//                    'NotIntLiteralParameterInArrayExpr'
//                )
//            	return
//    		}
//    	}
//    	
//    	if(e.parameters.length === 2) {
//    		val t2 = (t as SeqType).domain
//    		if(!(t instanceof SeqType)) {
//	    		error (
//	                    'The *variable* in an array expression with two indices must be a SeqType of SeqType, but it is SeqType of ' + t2.toString,
//	                    AssertionsPackage.Literals.ARRAY_EXPR__REF,
//	                    'NotSeqTypeInArrayExpr'
//	                )
//	            return
//	    	} 
//    	} else if(e.parameters.length === 1) {
//    		val t2 = (t as SeqType).domain
//    		if((t instanceof SeqType)) {
//	    		error (
//	                    'The *variable* in an array expression is a SeqType of SeqType, but only one index is provided' + t2.toString,
//	                    AssertionsPackage.Literals.ARRAY_EXPR__PARAMETERS,
//	                    'NotAllIndicesProvidedInArrayExpr'
//	                )
//	            return
//	    	} 
//    	}
//    }
//    
//    @Check
//    def checkSumArrayExpr(SumArrayExpr e) {
//    	val ref = getQNRefObj(e.ref)
//    	if(! (ref instanceof Variable)) {
//    		error (
//                    'The reference in a summation array expression must be a variable' + ref.toString,
//                    AssertionsPackage.Literals.SUM_ARRAY_EXPR__REF,
//                    'NotRef2VariableInSumArrayExpr'
//                )
//            return
//    	} 
//    	
//		val t = (ref as Variable).type
//		if(!(t instanceof SeqType)) {
//    		error (
//                    'The *variable* in a summation array expression must be a SeqType, but it is ' + t.toString,
//                    AssertionsPackage.Literals.SUM_ARRAY_EXPR__REF,
//                    'NotSeqTypeInSumArrayExpr'
//                )
//            return
//    	} 
//    	
//    	if(e.indices.length !== e.inds.length) {
//    		error (
//                    'Not all indices in a summation array expression are specified ',
//                    AssertionsPackage.Literals.SUM_ARRAY_EXPR__INDICES,
//                    'NotAllIndicesSpecifiedInSumArrayExpr'
//                )
//            return
//    	}
//    	
//    	if(e.indices.length === 2) {
//    		val t2 = (t as SeqType).domain
//    		if(!(t instanceof SeqType)) {
//	    		error (
//	                    'The *variable* in a summation array expression with two indices must be a SeqType of SeqType, but it is SeqType of ' + t2.toString,
//	                    AssertionsPackage.Literals.SUM_ARRAY_EXPR__REF,
//	                    'NotSeqTypeInSumArrayExpr'
//	                )
//	            return
//	    	} 
//    	} else if(e.indices.length === 1) {
//    		val t2 = (t as SeqType).domain
//    		if((t instanceof SeqType)) {
//	    		error (
//	                    'The *variable* in an array expression is a SeqType of SeqType, but only one index is provided' + t2.toString,
//	                    AssertionsPackage.Literals.SUM_ARRAY_EXPR__INDICES,
//	                    'NotAllIndicesProvidedInSumArrayExpr'
//	                )
//	            return
//	    	} 
//    	}
//    	
//    	for(ind: e.inds) {
//    		if(!e.indices.contains(ind.index)) {
//    			error (
//	                    'The index [' + ind.index + '] is not specified in a summation array expression, and only one of ' + e.indices.toString + "are allowed",
//	                    AssertionsPackage.Literals.SUM_ARRAY_EXPR__INDS,
//	                    'NotExistingIndicesInSumArrayExpr'
//	                )
//	            return
//    		}
//    	}
//    }

	/*
	@Check
    def checkBooleanExprInStateFormula(AndStateFormula pf) {
    	if(pf.left instanceof BracketTerminalFormula) {
    		var f = (pf.left as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The left expression in a && state formula must be boolean',
	                    AssertionsPackage.Literals.AND_STATE_FORMULA__LEFT,
	                    'NotBooleanExprAroundAndStateFormula'
	                )
	    		}
	    	}
	    }
	    
	    if(pf.right instanceof BracketTerminalFormula) {
    		var f = (pf.right as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The right expression in a && state formula must be boolean',
	                    AssertionsPackage.Literals.AND_STATE_FORMULA__RIGHT,
	                    'NotBooleanExprAroundAndStateFormula'
	                )
	    		}
	    	}
	    }
    }
    
    @Check
    def checkBooleanExprInStateFormula(OrStateFormula pf) {
    	if(pf.left instanceof BracketTerminalFormula) {
    		var f = (pf.left as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The left expression in a || state formula must be boolean',
	                    AssertionsPackage.Literals.OR_STATE_FORMULA__LEFT,
	                    'NotBooleanExprAroundOrStateFormula'
	                )
	    		}
	    	}
	    }
	    
	    if(pf.right instanceof BracketTerminalFormula) {
    		var f = (pf.right as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The right expression in a || state formula must be boolean',
	                    AssertionsPackage.Literals.OR_STATE_FORMULA__RIGHT,
	                    'NotBooleanExprAroundOrStateFormula'
	                )
	    		}
	    	}
	    }
    }
    
    @Check
    def checkBooleanExprInStateFormula(ImpliesStateFormula pf) {
    	if(pf.left instanceof BracketTerminalFormula) {
    		var f = (pf.left as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The left expression in a ==> state formula must be boolean',
	                    AssertionsPackage.Literals.IMPLIES_STATE_FORMULA__LEFT,
	                    'NotBooleanExprAroundImpliesStateFormula'
	                )
	    		}
	    	}
	    }
	    
	    if(pf.right instanceof BracketTerminalFormula) {
    		var f = (pf.right as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The right expression in a ==> state formula must be boolean',
	                    AssertionsPackage.Literals.IMPLIES_STATE_FORMULA__RIGHT,
	                    'NotBooleanExprAroundImpliesStateFormula'
	                )
	    		}
	    	}
	    }
    }
    
    @Check
    def checkBooleanExprInStateFormula(IffStateFormula pf) {
    	if(pf.left instanceof BracketTerminalFormula) {
    		var f = (pf.left as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The left expression in a <==> state formula must be boolean',
	                    AssertionsPackage.Literals.IFF_STATE_FORMULA__LEFT,
	                    'NotBooleanExprAroundIffStateFormula'
	                )
	    		}
	    	}
	    }
	    
	    if(pf.right instanceof BracketTerminalFormula) {
    		var f = (pf.right as BracketTerminalFormula)
    		if(f.form instanceof ProbFormulaExpr) {
	    		if(!isBooleanExpr((f.form as ProbFormulaExpr).expr)) {
	    			error (
	                    'The right expression in a <==> state formula must be boolean',
	                    AssertionsPackage.Literals.IFF_STATE_FORMULA__RIGHT,
	                    'NotBooleanExprAroundIffStateFormula'
	                )
	    		}
	    	}
	    }
    }
    */
    
	/////////////////////////// Probabilistic expressions [START] ///////////////////////////////////////////////
   	@Check
	def checkPExpression(pIff pf) {
		if(!isBooleanExpr(pf.left)) {
			error (
	            'The left expression in [iff] must be boolean',
	            AssertionsPackage.Literals.PIFF__LEFT,
	            'NotBooleanInIffExpr'
	        )
		}
		
		if(!isBooleanExpr(pf.right)) {
			error (
	            'The right expression in [iff] must be boolean',
	            AssertionsPackage.Literals.PIFF__RIGHT,
	            'NotBooleanInIffExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pImplies pf) {
		if(!isBooleanExpr(pf.left)) {
			error (
	            'The left expression in [=>] must be boolean',
	            AssertionsPackage.Literals.PIMPLIES__LEFT,
	            'NotBooleanInImpliesExpr'
	        )
		}
		
		if(!isBooleanExpr(pf.right)) {
			error (
	            'The right expression in [=>] must be boolean',
	            AssertionsPackage.Literals.PIMPLIES__RIGHT,
	            'NotBooleanInImpliesExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pAnd pf) {
		if(!isBooleanExpr(pf.left)) {
			error (
	            'The left expression in [/\\] must be boolean',
	            AssertionsPackage.Literals.PAND__LEFT,
	            'NotBooleanInAndExpr'
	        )
		}
		
		if(!isBooleanExpr(pf.right)) {
			error (
	            'The right expression in [/\\] must be boolean',
	            AssertionsPackage.Literals.PAND__RIGHT,
	            'NotBooleanInAndExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pOr pf) {
		if(!isBooleanExpr(pf.left)) {
			error (
	            'The left expression in [\\/] must be boolean',
	            AssertionsPackage.Literals.POR__LEFT,
	            'NotBooleanInOrExpr'
	        )
		}
		
		if(!isBooleanExpr(pf.right)) {
			error (
	            'The right expression in [\\/] must be boolean',
	            AssertionsPackage.Literals.POR__RIGHT,
	            'NotBooleanInOrExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pNot pf) {
		if(!isBooleanExpr(pf.exp)) {
			error (
	            'The expression in [not] must be boolean',
	            AssertionsPackage.Literals.PNOT__EXP,
	            'NotBooleanInNotExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pIfExpression pf) {
		if(!isBooleanExpr(pf.condition)) {
			error (
	            'The condition expression in [if cond then e1 else e2 end] must be boolean',
	            AssertionsPackage.Literals.PIF_EXPRESSION__CONDITION,
	            'NotBooleanInOrExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pGreaterThan pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [>] must be a number expression',
	            AssertionsPackage.Literals.PGREATER_THAN__LEFT,
	            'NotNumberInGreaterThanExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [>] must be a number expression',
	            AssertionsPackage.Literals.PGREATER_THAN__RIGHT,
	            'NotNumberInGreaterThanExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pGreaterOrEqual pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [>=] must be a number expression',
	            AssertionsPackage.Literals.PGREATER_OR_EQUAL__LEFT,
	            'NotNumberInGreaterOrEqualExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [>=] must be a number expression',
	            AssertionsPackage.Literals.PGREATER_OR_EQUAL__RIGHT,
	            'NotNumberInGreaterOrEqualExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pLessThan pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [<] must be a number expression',
	            AssertionsPackage.Literals.PLESS_THAN__LEFT,
	            'NotNumberInLessThanExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [<] must be a number expression',
	            AssertionsPackage.Literals.PLESS_THAN__RIGHT,
	            'NotNumberInLessThanExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pLessOrEqual pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [<=] must be a number expression',
	            AssertionsPackage.Literals.PLESS_OR_EQUAL__LEFT,
	            'NotNumberInLessOrEqualExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [<=] must be a number expression',
	            AssertionsPackage.Literals.PLESS_OR_EQUAL__RIGHT,
	            'NotNumberInLessOrEqualExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pPlus pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [+] must be a number expression',
	            AssertionsPackage.Literals.PPLUS__LEFT,
	            'NotNumberInPlusExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [+] must be a number expression',
	            AssertionsPackage.Literals.PPLUS__RIGHT,
	            'NotNumberInPlusExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pMinus pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [-] must be a number expression',
	            AssertionsPackage.Literals.PMINUS__LEFT,
	            'NotNumberInMinusExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [-] must be a number expression',
	            AssertionsPackage.Literals.PMINUS__RIGHT,
	            'NotNumberInMinusExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pMult pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [*] must be a number expression',
	            AssertionsPackage.Literals.PMULT__LEFT,
	            'NotNumberInMultExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [*] must be a number expression',
	            AssertionsPackage.Literals.PMULT__RIGHT,
	            'NotNumberInMultExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pDiv pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [/] must be a number expression',
	            AssertionsPackage.Literals.PDIV__LEFT,
	            'NotNumberInDivExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [/] must be a number expression',
	            AssertionsPackage.Literals.PDIV__RIGHT,
	            'NotNumberInDivExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pModulus pf) {
		if(!isNumExpr(pf.left)) {
			error (
	            'The left expression in [%] must be a number expression',
	            AssertionsPackage.Literals.PMODULUS__LEFT,
	            'NotNumberInModulusExpr'
	        )
		}
		
		if(!isNumExpr(pf.right)) {
			error (
	            'The right expression in [%] must be a number expression',
	            AssertionsPackage.Literals.PMODULUS__RIGHT,
	            'NotNumberInModulusExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pNeg pf) {
		if(!isNumExpr(pf.exp)) {
			error (
	            'The expression in [-] must be a number expression',
	            AssertionsPackage.Literals.PNEG__EXP,
	            'NotNumberInNegExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(pArrayExp pf) {
		for(p : pf.parameters) {
			if(!isNumExpr(p)) {
				error (
		            'The index parameter for an array [' + pf.value + '] must be a number expression',
		            AssertionsPackage.Literals.PARRAY_EXP__PARAMETERS,
		            'NotNumberInArrayIndexExpr'
		        )
			}
		}
	}
	
	@Check
	def checkPExpression(pSetRanges pf) {
		if(!isNumExpr(pf.start)) {
			error (
	            'The start of a set range [{_ to _ by step _}] must be a number expression',
	            AssertionsPackage.Literals.PSET_RANGES__START,
	            'NotNumberInStartOfSetRangeExpr'
	        )
		}
		
		if(!isNumExpr(pf.end)) {
			error (
	            'The end of a set range [{_ to _ by step _}] must be a number expression',
	            AssertionsPackage.Literals.PSET_RANGES__END,
	            'NotNumberInEndOfSetRangeExpr'
	        )
		}
		
		if(pf.step !== null && !isNumExpr(pf.step)) {
			error (
	            'The step of a set range [{_ to _ by step _}] must be a number expression',
	            AssertionsPackage.Literals.PSET_RANGES__STEP,
	            'NotNumberInStepOfSetRangeExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(UntilFormula pf) {
		if(!isBooleanExpr(pf.left)) {
			error (
	            'The left expression in [Until] must be boolean',
	            AssertionsPackage.Literals.UNTIL_FORMULA__LEFT,
	            'NotBooleanInUntilFormulaExpr'
	        )
		}
		
		if(!isBooleanExpr(pf.right)) {
			error (
	            'The right expression in [Until] must be boolean',
	            AssertionsPackage.Literals.UNTIL_FORMULA__RIGHT,
	            'NotBooleanInUntilFormulaExpr'
	        )
		}
	}
	
	@Check
	def checkPExpression(PFormula pf) {
		if(!isBooleanExpr(pf.fp)) {
			error (
	            'The expression in [Prob of [_]] must be boolean',
	            AssertionsPackage.Literals.PFORMULA__FP,
	            'NotBooleanInPathFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(RFormula pf) {
		if(!isBooleanExpr(pf.fr)) {
			error (
	            'The expression in [Reward of [_]] must be boolean',
	            AssertionsPackage.Literals.RFORMULA__FR,
	            'NotBooleanInRPathFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(AFormula pf) {
		if(!isBooleanExpr(pf.fa)) {
			error (
	            'The expression in [Forall [_]] must be boolean',
	            AssertionsPackage.Literals.AFORMULA__FA,
	            'NotBooleanInAFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(EFormula pf) {
		if(!isBooleanExpr(pf.fa)) {
			error (
	            'The expression in [Exists [_]] must be boolean',
	            AssertionsPackage.Literals.EFORMULA__FA,
	            'NotBooleanInEFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(NextFormula pf) {
		if(!isBooleanExpr(pf.f)) {
			error (
	            'The expression in [Next _] must be boolean',
	            AssertionsPackage.Literals.NEXT_FORMULA__F,
	            'NotBooleanInNextFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(FinalFormula pf) {
		if(!isBooleanExpr(pf.f)) {
			error (
	            'The expression in [Finally _] must be boolean',
	            AssertionsPackage.Literals.FINAL_FORMULA__F,
	            'NotBooleanInFinalFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(GlobalFormula pf) {
		if(!isBooleanExpr(pf.f)) {
			error (
	            'The expression in [Globally _] must be boolean',
	            AssertionsPackage.Literals.GLOBAL_FORMULA__F,
	            'NotBooleanInGlobalFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(WeakFormula pf) {
		if(!isBooleanExpr(pf.f)) {
			error (
	            'The expression in [Weak Until _] must be boolean',
	            AssertionsPackage.Literals.WEAK_FORMULA__F,
	            'NotBooleanInWeakFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(RelFormula pf) {
		if(!isBooleanExpr(pf.f)) {
			error (
	            'The expression in [Release _] must be boolean',
	            AssertionsPackage.Literals.REL_FORMULA__F,
	            'NotBooleanInReleaseFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(ReachAwardFormula pf) {
		if(!isBooleanExpr(pf.st1)) {
			error (
	            'The expression in [Reachable _] must be boolean',
	            AssertionsPackage.Literals.REACH_AWARD_FORMULA__ST1,
	            'NotBooleanInReachableFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(CumulAwardFormula pf) {
		if(!isBooleanExpr(pf.exp)) {
			error (
	            'The expression in [Cumul _] must be boolean',
	            AssertionsPackage.Literals.CUMUL_AWARD_FORMULA__EXP,
	            'NotBooleanInCumulFormula'
	        )
		}
	}
	
	@Check
	def checkPExpression(TotalAwardFormula pf) {
//		if(!isBooleanExpr(pf.exp)) {
//			error (
//	            'The expression in [Total _] must be boolean',
//	            AssertionsPackage.Literals.TOTAL_AWARD_FORMULA__EXP,
//	            'NotBooleanInTotalFormula'
//	        )
//		}
	}
	
	@Check
	def checkPExpression(UseMethod pf) {
		if(!isNumExpr(pf.pexp)) {
			error (
	            'The expression for [pathlen = _] must be a number expression',
	            AssertionsPackage.Literals.USE_METHOD__PEXP,
	            'NotNumberInPathlen'
	        )
		}
	}
	
	@Check
	def checkPExpression(CiMethod pf) {
		if(pf.w !== null && !isNumExpr(pf.w)) {
			error (
	            'The expression for [w = _] must be a number expression',
	            AssertionsPackage.Literals.CI_METHOD__W,
	            'NotNumberInWCiMethod'
	        )
		}
		
		if(pf.alpha !== null && !isNumExpr(pf.alpha)) {
			error (
	            'The expression for [alpha = _] must be a number expression',
	            AssertionsPackage.Literals.CI_METHOD__ALPHA,
	            'NotNumberInAlphaCiMethod'
	        )
		}
		
		if(pf.n !== null && !isNumExpr(pf.n)) {
			error (
	            'The expression for [n = _] must be a number expression',
	            AssertionsPackage.Literals.CI_METHOD__N,
	            'NotNumberInNCiMethod'
	        )
		}
	}
	
	@Check
	def checkPExpression(APMCMethod pf) {
		if(pf.epsilon !== null && !isNumExpr(pf.epsilon)) {
			error (
	            'The expression for [epsilon = _] must be a number expression',
	            AssertionsPackage.Literals.APMC_METHOD__EPSILON,
	            'NotNumberInEpsilonAPMCMethod'
	        )
		}
		
		if(pf.delta !== null && !isNumExpr(pf.delta)) {
			error (
	            'The expression for [delta = _] must be a number expression',
	            AssertionsPackage.Literals.APMC_METHOD__DELTA,
	            'NotNumberInDeltaAPMCMethod'
	        )
		}
		
		if(pf.n !== null && !isNumExpr(pf.n)) {
			error (
	            'The expression for [n = _] must be a number expression',
	            AssertionsPackage.Literals.APMC_METHOD__N,
	            'NotNumberInNAPMCMethod'
	        )
		}
	}
	
	@Check
	def checkPExpression(SPRTMethod pf) {
		if(pf.alpha !== null && !isNumExpr(pf.alpha)) {
			error (
	            'The expression for [alpha = _] must be a number expression',
	            AssertionsPackage.Literals.SPRT_METHOD__ALPHA,
	            'NotNumberInALPHASPRTMethod'
	        )
		}
		
		if(pf.delta !== null && !isNumExpr(pf.delta)) {
			error (
	            'The expression for [delta = _] must be a number expression',
	            AssertionsPackage.Literals.SPRT_METHOD__DELTA,
	            'NotNumberInDeltaSPRTMethod'
	        )
		}
	}
	
	@Check
	def checkPExpression(Bound pf) {
		if(!isNumExpr(pf.exp)) {
			error (
	            'The expression in the bound [<|<=|>=|> _] must be a number expression',
	            AssertionsPackage.Literals.BOUND__EXP,
	            'NotNumberInBound'
	        )
		}
	}
	
	/////////////////////////// Probabilistic expressions [END] ///////////////////////////////////////////////
  	@Check
  	def checkPEvent(pEvent pe) {
//  		System.out.println("checkPEvent: " + printQNRef(pe.ref))
  		val event = getQNRefObj(pe.ref)
  		// the reference must be an event
  		if(!(event instanceof Event)) {
  			error (
	            'The reference must be an event',
	            AssertionsPackage.Literals.PEVENT__REF,
	            'NotEventInpEvent'
	        )
  		}
  		
		// the event must be used for input
		val parent = (pe.ref as QualifiedNameToElement).ref
		val parent_obj = getQNRefObj1(pe.ref)
		if(!(parent_obj instanceof RoboticPlatform || parent_obj instanceof StateMachine)) {
			error (
	            'The ConnectionNode that the event is from must be a robotic platform or a  state machine',
	            AssertionsPackage.Literals.PEVENT__REF,
	            'NotRPorSTMInpEvent'
	        )
		}
  		
  		val parent_of_parent_obj = getQNRefObj1(parent)
  		if((pe.dir instanceof PEventIN)) {
  			// the event must be used for input
  			if(parent_of_parent_obj instanceof RCModule) {
  				var mod = parent_of_parent_obj as RCModule
  				if(mod.connections.filter[c|(c.to===parent_obj || c.bidirec) && c.eto === event].isEmpty) {
	  				error (
			            'The event is not used for input in ' + parent_obj.name,
			            AssertionsPackage.Literals.PEVENT__DIR,
			            'EventNotUsedForInputInpEvent'
			        )
			     }
  			}
  			
  			if(parent_of_parent_obj instanceof ControllerDef) {
  				var ctrl = parent_of_parent_obj as ControllerDef
  				if(ctrl.connections.filter[c|(c.to===parent_obj || c.bidirec) && c.eto === event].isEmpty) {
	  				error (
			            'The event is not used for input in ' + parent_obj.name,
			            AssertionsPackage.Literals.PEVENT__DIR,
			            'EventNotUsedForInputInpEvent'
			        )
			     }
  			}
  			
  			if(parent_of_parent_obj instanceof ControllerRef) {
  				var ctrl = (parent_of_parent_obj as ControllerRef).ref
  				if(ctrl.connections.filter[c|(c.to===parent_obj || c.bidirec) && c.eto === event].isEmpty) {
	  				error (
			            'The event is not used for input in ' + parent_obj.name,
			            AssertionsPackage.Literals.PEVENT__DIR,
			            'EventNotUsedForInputInpEvent'
			        )
			     }
  			}
  		}
  		
  		if((pe.dir instanceof PEventOUT)) {
  			// the event must be used for input
  			if(parent_of_parent_obj instanceof RCModule) {
  				var mod = parent_of_parent_obj as RCModule
  				if(mod.connections.filter[c|(c.from===parent_obj || c.bidirec) && c.efrom === event].isEmpty) {
	  				error (
			            'The event is not used for output in ' + parent_obj.name,
			            AssertionsPackage.Literals.PEVENT__DIR,
			            'EventNotUsedForOutputInpEvent'
			        )
			     }
  			}
  			
  			if(parent_of_parent_obj instanceof ControllerDef) {
  				var ctrl = parent_of_parent_obj as ControllerDef
  				if(ctrl.connections.filter[c|(c.from===parent_obj || c.bidirec) && c.efrom === event].isEmpty) {
	  				error (
			            'The event is not used for output in ' + parent_obj.name,
			            AssertionsPackage.Literals.PEVENT__DIR,
			            'EventNotUsedForOutputInpEvent'
			        )
			     }
  			}
  			
  			if(parent_of_parent_obj instanceof ControllerRef) {
  				var ctrl = (parent_of_parent_obj as ControllerRef).ref
  				if(ctrl.connections.filter[c|(c.from===parent_obj || c.bidirec) && c.efrom === event].isEmpty) {
	  				error (
			            'The event is not used for output in ' + parent_obj.name,
			            AssertionsPackage.Literals.PEVENT__DIR,
			            'EventNotUsedForOutputInpEvent'
			        )
			     }
  			}
  		}
  	}
  	
  	@Check
  	def checkPEvent(pEventVal pe) {
  		val obj = getQNRefObj(pe.ref.ref)
  		if(obj instanceof Event) {
  			val event = obj as Event;
  			if(event.type === null) {
  				error (
			            'The event is a simple event, doesn\'t carry a message, and so [val] cannot be used.',
			            AssertionsPackage.Literals.PEVENT_VAL__REF,
			            'NotTypedEventInpEventVal'
			        )
  			}
  		}
  	}

// ///////////////////////// Probabilistic assertions [END] ///////////////////////////////////////////////
}
