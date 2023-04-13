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
 
package circus.robocalc.robochart.assertions.ui.contentassist

import circus.robocalc.robochart.assertions.assertions.Assertion
import circus.robocalc.robochart.assertions.assertions.AssertionParameter
import circus.robocalc.robochart.assertions.scoping.AssertionsScopeProvider
import com.google.inject.Inject
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.scoping.IScope
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor

/**
 * See https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#content-assist
 * on how to customize the content assistant.
 */
class AssertionsProposalProvider extends AbstractAssertionsProposalProvider {
	@Inject AssertionsScopeProvider sp;
	
	override completeAssertionParameter_Variable(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		super.completeAssertionParameter_Variable(model, assignment, context, acceptor)
	 	if (model instanceof AssertionParameter) {
	 		val assertion = model.eContainer as Assertion
	 		val constants = sp.constantsDeclared(assertion, IScope::NULLSCOPE)
	 		for (c: constants.allElements) {
	 			acceptor.accept(createCompletionProposal(c.name.lastSegment, context))
	 		}
	 	}	
	}
}
