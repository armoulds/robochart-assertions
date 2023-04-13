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
 
package circus.robocalc.robochart.assertions.generator

import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.ISafeRunnable
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.SafeRunner
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.generator.AbstractGenerator
import org.eclipse.xtext.generator.IFileSystemAccess2
import org.eclipse.xtext.generator.IGeneratorContext

/**
 * Generates code from your model files on save.
 * 
 * See https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#code-generation
 */
class AssertionsGenerator extends AbstractGenerator {

	static val GEN_ID = "robochart.generator"

	override void doGenerate(Resource resource, IFileSystemAccess2 fsa, IGeneratorContext context) {
		val config = Platform.extensionRegistry.getConfigurationElementsFor(GEN_ID);
		try {
			for (e : config) {
				val o = e.createExecutableExtension("class")
				if (o instanceof AbstractGenerator) {
					// executing generator
					val runnable = new ISafeRunnable() {
						override void handleException(Throwable e) {
							System.err.println(e.message)
						}

						override void run() throws Exception {
							(o as AbstractGenerator).doGenerate(resource, fsa, context)
						}
					}
					SafeRunner.run(runnable)
				}
			}
		} catch (CoreException ex) {
			System.err.println(ex.message)
		}
	}
}
