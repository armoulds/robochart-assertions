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
 
package circus.robocalc.robochart.assertions.scoping

import circus.robocalc.robochart.Controller
import circus.robocalc.robochart.ControllerDef
import circus.robocalc.robochart.ControllerRef
import circus.robocalc.robochart.Event
import circus.robocalc.robochart.NamedElement
import circus.robocalc.robochart.Operation
import circus.robocalc.robochart.OperationDef
import circus.robocalc.robochart.OperationRef
import circus.robocalc.robochart.OperationSig
import circus.robocalc.robochart.Parameter
import circus.robocalc.robochart.RCModule
import circus.robocalc.robochart.RCPackage
import circus.robocalc.robochart.RoboticPlatform
import circus.robocalc.robochart.RoboticPlatformDef
import circus.robocalc.robochart.RoboticPlatformRef
import circus.robocalc.robochart.StateMachine
import circus.robocalc.robochart.StateMachineDef
import circus.robocalc.robochart.StateMachineRef
import circus.robocalc.robochart.Variable
import circus.robocalc.robochart.State
import circus.robocalc.robochart.VariableModifier
import circus.robocalc.robochart.ConnectionNode
import circus.robocalc.robochart.PrimitiveType
import circus.robocalc.robochart.Enumeration
import circus.robocalc.robochart.Literal
import circus.robocalc.robochart.assertions.assertions.Assertion
import circus.robocalc.robochart.assertions.assertions.AssertionConstant
import circus.robocalc.robochart.assertions.assertions.AssertionParameter
import circus.robocalc.robochart.assertions.assertions.BinaryAssertion
import circus.robocalc.robochart.assertions.assertions.NameRef
import circus.robocalc.robochart.assertions.assertions.RCRef
import circus.robocalc.robochart.assertions.assertions.QualifiedNameToElement
import circus.robocalc.robochart.assertions.assertions.UnaryAssertion
import circus.robocalc.robochart.assertions.assertions.pModules
import circus.robocalc.robochart.assertions.assertions.pModule
import circus.robocalc.robochart.assertions.assertions.pVariableRef
import com.google.inject.Inject
import java.util.HashSet
import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.scoping.Scopes

import static circus.robocalc.robochart.assertions.assertions.AssertionsPackage.Literals.*

/**
 * This class contains custom scoping description.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#scoping
 * on how and when to use it.
 */
class AssertionsScopeProvider extends AbstractAssertionsScopeProvider {
	@Inject IQualifiedNameProvider qnp
	
	override getScope(EObject context, EReference reference) {
		if (context instanceof AssertionConstant) {
			if (reference == ASSERTION_CONSTANT__VARIABLE) {
/* TODO: this eventually need to be removed because all constants should be specified with context, not qualified names */				
//				if (context.context === null) {
//					val a = (context.eContainer as Assertion)
//					val s = a.constantsDeclared(IScope::NULLSCOPE)//super.delegateGetScope(context,reference))
//					return s
//				} else {
					val ctx = context.context
					return ctx.allConstants(IScope::NULLSCOPE)
//				}
			} 
		} else if (context instanceof AssertionParameter) {
			if (reference == ASSERTION_PARAMETER__VARIABLE) {
				val ctx = context.context
				if (ctx instanceof OperationRef) {
					return Scopes.scopeFor(ctx.ref.parameters)
				} else if (ctx instanceof OperationDef){
					return Scopes.scopeFor(ctx.parameters)
				}
			}
		} else if (context instanceof QualifiedNameToElement) {
			val head = context.ref;
	        switch (head) {
	        	// For the head, it might be NameRef starting with a RCModule or a variable
	            NameRef : {
	            	val name = head.name
	            	if(name instanceof RCModule) {
            			val scopeForConnection = Scopes::scopeFor(name.connections, IScope::NULLSCOPE)
						return Scopes::scopeFor(name.nodes, scopeForConnection)
	            	} else if(name instanceof RCPackage) {
						return Scopes::scopeFor(name.modules, IScope::NULLSCOPE)
	            	} else if (name instanceof Variable) {
	            		return delegateGetScope(name, reference)
					} else if (name instanceof PrimitiveType) {
	                    return IScope::NULLSCOPE
	                }/*else if(name instanceof Enumeration) {
						return Scopes::scopeFor(name.literals, IScope::NULLSCOPE)
	            	}*/ else {
                        if(name !== null) {
	                    	return delegateGetScope(name, reference)
                        } else 
                            return IScope::NULLSCOPE
	            	}
	            }
	            
	            RCRef : {
	            	val rc = head.rc
					var scopeForRC = IScope::NULLSCOPE
					if (rc.name !== null) {
//                    	System.out.println("RCPackage: " + rc.name.toString());
            			scopeForRC = Scopes::scopeFor(rc.modules, IScope::NULLSCOPE)
	            	} 
	            	return scopeForRC
	            }
				
				// QualifiedNameToElement
	            QualifiedNameToElement : {
	                val tail = head.tail
	                switch (tail) {
	                    Event : return IScope::NULLSCOPE
	                    Variable : {
	                    	return delegateGetScope(tail, reference)
	                    }
	                    RCModule: {
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.elementsDeclared(s)
	                    	return s
	                    }
	                    StateMachineDef: {
	                    	/*var s = delegateGetScope(tail, reference)
	                    	s = tail.eventsDeclared(s)
	                    	s = tail.variablesDeclared(s)
	                    	s = tail.constantsDeclared(s)*/
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.elementsDeclared(s)
	                    	return s
	                    }
	                    StateMachineRef: {
	                    	/*var s = delegateGetScope(tail.ref, reference)
	                    	s = tail.eventsDeclared(s)
	                    	s = tail.ref.variablesDeclared(s)
	                    	s = tail.ref.constantsDeclared(s)*/
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.ref.elementsDeclared(s)
	                    	return s
	                    }
	                    ControllerDef: {
	                    	/*var s = delegateGetScope(tail, reference)
	                    	s = tail.eventsDeclared(s)
	                    	s = tail.variablesDeclared(s)
	                    	s = tail.constantsDeclared(s)*/
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.elementsDeclared(s)
	                    	return s
	                    	
	                    }
	                    ControllerRef: {
	                    	/*var s = delegateGetScope(tail.ref, reference)
	                    	s = tail.eventsDeclared(s)
	                    	s = tail.ref.variablesDeclared(s)
	                    	s = tail.ref.constantsDeclared(s) */
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.ref.elementsDeclared(s)
	                    	return s	                    	
	                    }
	                    RoboticPlatformDef: {
	                    	/*var s = delegateGetScope(tail, reference)
	                    	s = tail.eventsDeclared(s)
	                    	s = tail.variablesDeclared1(s)
	                    	var s = tail.constantsDeclared(s)
	                    	return s	                   */
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.elementsDeclared(s)
	                    	
	                    	return s
	                    }
	                    RoboticPlatformRef: {
	                    	/*var s = delegateGetScope(tail.ref, reference)
	                    	s = tail.eventsDeclared(s)
	                    	s = tail.ref.variablesDeclared1(s)*/
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.ref.elementsDeclared(s)
	                    	/* s = tail.ref.constantsDeclared(s)*/
	                    
	                    	return s	                    	
	                    }
	                    State: {
	                    	var s = IScope::NULLSCOPE
	                    	s = tail.elementsDeclared(s)
	                    
	                    	return s	                    	
	                    }
	                    PrimitiveType: {
	                    	return IScope::NULLSCOPE
	                    }
//	                    Literal: {
//	                    	return delegateGetScope(tail, reference)
//	                    }
	                    default: {
	                    	if(tail !== null)
	                    		return delegateGetScope(tail, reference)
	                    	else 
	                    		return IScope::NULLSCOPE
	                    }
	                }
	            }
	             
	            default: return IScope::NULLSCOPE
	        }
		} else if (context instanceof pVariableRef) {
//			if(reference == PVARIABLE_REF__MODS) {
//				if(context.mods !== null && context.mods.name !== null) {
//					return Scopes::scopeFor(context.mods.modules, IScope::NULLSCOPE)
//				}
//			}
			
			if(reference == PVARIABLE_REF__MOD) {
				if(context.mods !== null && context.mods.name !== null
				) {
					return Scopes::scopeFor(context.mods.modules, IScope::NULLSCOPE)
				}
			}
			
			if(reference == PVARIABLE_REF__REF) {
				if(context.mod !== null && context.mod.name !== null
				) {
					return Scopes::scopeFor(context.mod.variables, IScope::NULLSCOPE)
				}
			}
		} 
		return super.getScope(context,reference)
	}
	
	def dispatch IScope parametersDeclared(UnaryAssertion m, IScope parent) {
		val base = QualifiedName.create()
		var scope = m.element.allParameters(base,parent)
		return scope
		
	}
	def dispatch IScope parametersDeclared(BinaryAssertion m, IScope parent) {
		val base = QualifiedName.create()
		var scope = m.right.allParameters(base,parent)
		scope = m.left.allParameters(base,scope)
		return scope
	}
	
	def dispatch IScope allParameters(OperationSig o, QualifiedName base, IScope parent) {
		val qn = base.append(o.name)
		val params = new HashSet<Parameter>()
		params.addAll(o.parameters)
		Scopes::scopeFor(
			params,
			[x|
				qn.append(x.name)
			],
			parent
		)
	}
	
	def dispatch IScope allParameters(EObject o, QualifiedName base, IScope parent) {
		parent
	}
	
	def dispatch IScope constantsDeclared(UnaryAssertion m, IScope parent) {
		val el = m.element
		val base = QualifiedName.create()
		el.requiredConstants(base,el.allLocalConstants(base,parent))
		
	}
	def dispatch IScope constantsDeclared(BinaryAssertion m, IScope parent) {
		val l = m.left
		val r = m.right
		val base = QualifiedName.create()
		l.requiredConstants(
			base, 
			r.requiredConstants(
				base, 
				l.allLocalConstants(
					base, 
					r.allLocalConstants(base, parent)
				)
			)
		)
	}
	
	def dispatch IScope allLocalConstants(RCModule m, QualifiedName base, IScope parent) {
		val qn = base.append(m.name)
		var scope = parent
		for (c: m.nodes) {
			scope = c.allLocalConstants(qn,scope)
		}
		scope
	}
	
	def dispatch IScope allLocalConstants(RoboticPlatform rp, QualifiedName base, IScope parent) {
		var scope = parent
		val rpdef = if (rp instanceof RoboticPlatformRef) rp.ref else rp as RoboticPlatformDef
		val qn = base.append(rpdef.name)
		for (i: rpdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		for (i: rpdef.PInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		
		val consts = new HashSet<Variable>()
		rpdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
		scope = Scopes::scopeFor(
			consts,
			[x|
				qn.append(x.name)
			],
			scope
		)
		
		scope
	}
	
	def dispatch IScope allLocalConstants(Controller c, QualifiedName base, IScope parent) {
		var scope = parent
		val cdef = if (c instanceof ControllerRef) c.ref else c as ControllerDef
		val qn = base.append(cdef.name)
		for (s: cdef.machines) {
			scope = s.allLocalConstants(qn, scope)
		}
		for (o: cdef.LOperations) {
			scope = o.allLocalConstants(qn, scope)
		}
		for (i: cdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		val consts = new HashSet<Variable>()
		cdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
		scope = Scopes::scopeFor(
			consts,
			[x|
				qn.append(x.name)
			],
			scope
		)
		
		scope
	}
	
	def dispatch IScope allLocalConstants(StateMachine s, QualifiedName base, IScope parent) {
		var scope = parent
		val sdef = if (s instanceof StateMachineRef) s.ref else s as StateMachineDef
		val qn = base.append(s.name)
		for (i: sdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|
					consts.addAll(vl.vars.filter[v|v.initial === null])
				]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		val consts = new HashSet<Variable>()
		sdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|
				consts.addAll(vl.vars.filter[v|v.initial === null])
			]
		scope = Scopes::scopeFor(
			consts,
			[x|
				qn.append(x.name)
			],
			scope
		)
		return scope
	}
	
	def dispatch IScope allLocalConstants(Operation s, QualifiedName base, IScope parent) {
		var scope = parent
		val sdef = if (s instanceof OperationRef) s.ref else s as OperationDef
		val qn = base.append(sdef.name)
		for (i: sdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		val consts = new HashSet<Variable>()
		sdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
		scope = Scopes::scopeFor(
			consts,
			[x|
				qn.append(x.name)
			],
			scope
		)
		return scope
	}
	
	def dispatch IScope allLocalConstants(EObject o, QualifiedName base, IScope parent) {
		parent
	}
	
	/* all required variables (only the top element) */
	
	def dispatch IScope requiredConstants(Controller c, QualifiedName base, IScope parent) {
		var scope = parent
		val cdef = if (c instanceof ControllerRef) c.ref else c as ControllerDef
		val qn = base.append(cdef.name)
		for (i: cdef.RInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars)]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		scope
	}
	
	def dispatch IScope requiredConstants(StateMachine s, QualifiedName base, IScope parent) {
		var scope = parent
		val sdef = if (s instanceof StateMachineRef) s.ref else s as StateMachineDef
		val qn = base.append(s.name)
		for (i: sdef.RInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars)]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		return scope
	}
	
	def dispatch IScope requiredConstants(Operation s, QualifiedName base, IScope parent) {
		var scope = parent
		val sdef = if (s instanceof OperationRef) s.ref else s as OperationDef
		val qn = base.append(sdef.name)
		for (i: sdef.RInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars)]
			scope = Scopes::scopeFor(
				consts,
				[x|
					qn.append(x.name)
				],
				scope
			)
		}
		return scope
	}
	
	def dispatch IScope requiredConstants(EObject o, QualifiedName base, IScope parent) {
		parent
	}
	
	
	    /**
     * Get all state machine references to the state machine definition
     * @param obj
     * @param refs
     */
    private def getAllRefs(StateMachineDef obj, List<StateMachineRef> refs) {
        val rootElement = EcoreUtil2.getRootContainer(obj);
        val candidates =
                EcoreUtil2.getAllContentsOfType(rootElement, StateMachineRef);
        for(StateMachineRef c : candidates) {
            // System.out.println("[StateMachineRef]getAllRefs: " + c.toString());
            if(c.getRef() != null && c.getRef() == obj) {
                refs.add(c);
            }
        }
    }

    /**
     * Get all controllers references to the controller definition
     * @param obj
     * @param refs
     */
    private def getAllRefs(ControllerDef obj, List<ControllerRef> refs) {
        val rootElement = EcoreUtil2.getRootContainer(obj);
        val candidates =
                EcoreUtil2.getAllContentsOfType(rootElement, ControllerRef);
        for(ControllerRef c : candidates) {
//          System.out.println("[ControllerRef]getAllRefs: " + c.toString());
            if(c.getRef() != null && c.getRef() == obj) {
                refs.add(c);
            }
        }
    }
    
	def dispatch IScope allConstants(Controller c, IScope parent) {
		var scope = parent
		val cdef = if (c instanceof ControllerRef) c.ref else c as ControllerDef
		
		for (i: cdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(consts, scope)
		}
		for (i: cdef.RInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(consts, scope)
		}
		val consts = new HashSet<Variable>()
		cdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
		scope = Scopes::scopeFor(consts, scope)
		
		scope
	}
	
	def dispatch IScope allConstants(StateMachine s, IScope parent) {
		var scope = parent
		val sdef = if (s instanceof StateMachineRef) s.ref else s as StateMachineDef
		
		for (i: sdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|
					consts.addAll(vl.vars.filter[v|v.initial === null])
				]
			scope = Scopes::scopeFor(consts, scope)
		}
		for (i: sdef.RInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|
					consts.addAll(vl.vars.filter[v|v.initial === null])
				]
			scope = Scopes::scopeFor(consts, scope)
		}
		val consts = new HashSet<Variable>()
		sdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|
				consts.addAll(vl.vars.filter[v|v.initial === null])
			]
		scope = Scopes::scopeFor(consts, scope)
		return scope
	}
	
	def dispatch IScope allConstants(Operation s, IScope parent) {
		var scope = parent
		val sdef = if (s instanceof OperationRef) s.ref else s as OperationDef
		
		for (i: sdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(consts, scope)
		}
		for (i: sdef.RInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(consts, scope)
		}
		val consts = new HashSet<Variable>()
		sdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
		scope = Scopes::scopeFor(consts, scope)
		return scope
	}
	
	def dispatch IScope allConstants(RCModule m, IScope parent) {
		var scope = parent
		return (m.nodes.findFirst[x|x instanceof RoboticPlatform] as RoboticPlatform).allConstants(scope)
	}
	
	def dispatch IScope allConstants(RoboticPlatform rp, IScope parent) {
		var scope = parent
		val rpdef = if (rp instanceof RoboticPlatformRef) rp.ref else rp as RoboticPlatformDef
		for (i: rpdef.interfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(consts, scope)
		}
		for (i: rpdef.PInterfaces) {
			val consts = new HashSet<Variable>()
			i.variableList
				.filter[vl|vl.modifier === VariableModifier.CONST]
				.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
			scope = Scopes::scopeFor(consts, scope)
		}
		
		val consts = new HashSet<Variable>()
		rpdef.variableList
			.filter[vl|vl.modifier === VariableModifier.CONST]
			.forEach[vl|consts.addAll(vl.vars.filter[v|v.initial === null])]
		scope = Scopes::scopeFor(consts, scope)
		
		scope
	}
	
	def dispatch IScope allConstants(NamedElement c, IScope parent) {
		return parent
	}

	def dispatch IScope variablesDeclared1(RoboticPlatformDef ndef, IScope p) {
		var s = p
		for (l : ndef.variableList) {
			s = Scopes::scopeFor(l.vars, s)
		}

		for (i : ndef.PInterfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		}

		return s
	}

	def dispatch IScope elementsDeclared(RoboticPlatformDef ndef, IScope p) {
        var s = p
		for (l : ndef.variableList) {
			s = Scopes::scopeFor(l.vars, s)
		}

		s = Scopes::scopeFor(ndef.events, s)

		for (i : ndef.PInterfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		}

		for (i : ndef.interfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		    s = Scopes::scopeFor(i.events, s)
		}

        return s
	}

	def dispatch IScope elementsDeclared(StateMachineDef ndef, IScope p) {
        var s = p
		for (l : ndef.variableList) {
			s = Scopes::scopeFor(l.vars, s)
		}

		s = Scopes::scopeFor(ndef.events, s)

		for (i : ndef.PInterfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		}

		for (i : ndef.interfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		    s = Scopes::scopeFor(i.events, s)
		}

        s = Scopes::scopeFor(ndef.nodes, s)

        s = Scopes::scopeFor(ndef.transitions, s)

        return s
	}

	def dispatch IScope elementsDeclared(State ndef, IScope p) {
        var s = p

        s = Scopes::scopeFor(ndef.nodes, s)

        s = Scopes::scopeFor(ndef.transitions, s)

        return s
	}

	def dispatch IScope elementsDeclared(ControllerDef ndef, IScope p) {
        var s = p
		for (l : ndef.variableList) {
			s = Scopes::scopeFor(l.vars, s)
		}

		s = Scopes::scopeFor(ndef.events, s)

		for (i : ndef.PInterfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		}

		for (i : ndef.interfaces) {
			for (l : i.variableList) {
				s = Scopes::scopeFor(l.vars, s)
			}
		    s = Scopes::scopeFor(i.events, s)
		}
        
        s = Scopes::scopeFor(ndef.machines, s)

        return s
	}
	
	def dispatch IScope elementsDeclared(RCModule ndef, IScope p) {
        var s = p
        s = Scopes::scopeFor(ndef.connections, s)
		s = Scopes::scopeFor(ndef.nodes, s)
		
        return s
	}
}
