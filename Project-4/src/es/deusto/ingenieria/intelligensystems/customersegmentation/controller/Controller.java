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
package es.deusto.ingenieria.intelligensystems.customersegmentation.controller;

import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Gender;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.GraduationStatus;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.MaritalStatus;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Profession;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Segmentation;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.SpendingScore;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Var1Category;

public class Controller {
	public Segmentation predict(
			Gender gender,
			MaritalStatus everMarried,
			int age,
			GraduationStatus graduated,
			Profession profession,
			int workExperience,
			SpendingScore spendingScore,
			int familySize,
			Var1Category var1) {

		/*
		 * Enter here the rules that are obtained from the decision tree you have
		 * created with the R script.
		 * 
		 * The class Customer defines all attributes of each person. For each qualitative
		 * attribute enumerations have been defined.
		 * 
		 * If you have defined ranges for the numeric attributes, add in this class the
		 * enumerations to translate numeric values into qualitative values.
		 * 
		 * DO NOT MODIFY EITHER THE CUSTOMER CLASS OR THE GUI.
		 */
		if (profession == Profession.ARTIST && spendingScore == SpendingScore.LOW && age <= 41) {
			return Segmentation.A;
		}

		if ((profession == Profession.DOCTOR || profession == Profession.ENGINEER || profession == Profession.ENTERTAINMENT || profession == Profession.EXECUTIVE || profession == Profession.LAWYER || profession == Profession.HOMEMAKER)
		 && spendingScore == SpendingScore.LOW && age >= 31 && age <= 89) {
			return Segmentation.A;
		}

		if ((profession == Profession.DOCTOR || profession == Profession.ENGINEER || profession == Profession.ENTERTAINMENT || profession == Profession.EXECUTIVE || profession == Profession.LAWYER || profession == Profession.HOMEMAKER)
		 && (spendingScore == SpendingScore.AVERAGE || spendingScore == SpendingScore.HIGH)) {
			return Segmentation.B;
		}

		if (profession == Profession.ARTIST && spendingScore == SpendingScore.LOW && age >= 42 && age <= 89) {
			return Segmentation.B;
		}

		if (profession == Profession.ARTIST && (spendingScore == SpendingScore.AVERAGE || spendingScore == SpendingScore.HIGH)) {
			return Segmentation.C;
		}

		if ((profession == Profession.DOCTOR || profession == Profession.ENGINEER || profession == Profession.ENTERTAINMENT || profession == Profession.EXECUTIVE || profession == Profession.LAWYER || profession == Profession.HOMEMAKER)
		 && spendingScore == SpendingScore.LOW && age >= 18 && age <= 31) {
			return Segmentation.D;
		}

		if (profession == Profession.HEALTHCARE || profession == Profession.MARKETING) {
			return Segmentation.D;
		}
		
		return null;
	}
}
