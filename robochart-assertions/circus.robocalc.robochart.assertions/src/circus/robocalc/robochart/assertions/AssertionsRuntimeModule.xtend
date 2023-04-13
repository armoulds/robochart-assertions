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

/*
 * generated by Xtext 2.17.1
 */
package circus.robocalc.robochart.assertions

import circus.robocalc.robochart.textual.RoboChartOutputConfigurationProvider
import circus.robocalc.robochart.textual.RoboChartQualifiedNameConverter
import circus.robocalc.robochart.textual.scoping.RoboChartImportURIGlobalScopeProvider
import org.eclipse.xtext.generator.IOutputConfigurationProvider
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.scoping.IGlobalScopeProvider

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
class AssertionsRuntimeModule extends AbstractAssertionsRuntimeModule {
	override Class<? extends IGlobalScopeProvider> bindIGlobalScopeProvider() {
    	return RoboChartImportURIGlobalScopeProvider
	}
	def Class<? extends IQualifiedNameConverter> bindIQualifiedNameConverter() {
		return RoboChartQualifiedNameConverter
	}
	def Class<? extends IOutputConfigurationProvider> bindIOutputConfigurationProvider() {
		return RoboChartOutputConfigurationProvider
	}
}