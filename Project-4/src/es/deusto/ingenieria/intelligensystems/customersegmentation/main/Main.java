/*
 # -----------------------------------------------------------------------------
 # This code snippet was generated for educational purposes as part of the
 # Intelligent Systems course at the University of Deusto. The code has been
 # created with assistance from ChatGPT version 3.5 and GitHub Copilot.
 #
 # The code is released under the Creative Commons License and is provided
 # for free use and modification by the programming and development community.
 #
 # This script was generated in April 2024, the year when the 'Athletic Club de
 # Bilbao' won the 25th King's Cup.
 # -----------------------------------------------------------------------------
 */

package es.deusto.ingenieria.intelligensystems.customersegmentation.main;

import javax.swing.SwingUtilities;

import es.deusto.ingenieria.intelligensystems.customersegmentation.controller.Controller;
import es.deusto.ingenieria.intelligensystems.customersegmentation.gui.MainWindow;

public class Main {

	public static void main(String[] args) {
		SwingUtilities.invokeLater(() -> new MainWindow(new Controller()));
	}
}