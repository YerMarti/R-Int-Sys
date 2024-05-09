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

package es.deusto.ingenieria.intelligensystems.customersegmentation.domain;

public class Customer {

	public enum Gender {
		MALE, FEMALE
	}

	public enum MaritalStatus {
		YES, NO
	}

	public enum GraduationStatus {
		YES, NO
	}

	public enum Profession {
		ARTIST, 
		DOCTOR,
		ENGINEER,
		ENTERTAINMENT,
		EXECUTIVE,
		HEALTHCARE,
		HOMEMAKER, 
		LAWYER, 
		MARKETING
	}

	public enum SpendingScore {
		LOW, AVERAGE, HIGH
	}

	public enum Var1Category {
		CAT_1, CAT_2, CAT_3, CAT_4, CAT_5, CAT_6, CAT_7
	}

	public enum Segmentation {
		A, B, C, D
	}

	private int id;
	private Gender gender;
	private MaritalStatus everMarried;
	private int age;
	private GraduationStatus graduated;
	private Profession profession;
	private int workExperience;
	private SpendingScore spendingScore;
	private int familySize;
	private Var1Category var1;
	private Segmentation segmentation;

	public Customer(int id, Gender gender, MaritalStatus everMarried, int age, GraduationStatus graduated,
			Profession profession, int workExperience, SpendingScore spendingScore, int familySize, Var1Category var1,
			Segmentation segmentation) {
		super();
		this.id = id;
		this.gender = gender;
		this.everMarried = everMarried;
		this.age = age;
		this.graduated = graduated;
		this.profession = profession;
		this.workExperience = workExperience;
		this.spendingScore = spendingScore;
		this.familySize = familySize;
		this.var1 = var1;
		this.segmentation = segmentation;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public Gender getGender() {
		return gender;
	}

	public void setGender(Gender gender) {
		this.gender = gender;
	}

	public MaritalStatus getEverMarried() {
		return everMarried;
	}

	public void setEverMarried(MaritalStatus everMarried) {
		this.everMarried = everMarried;
	}

	public int getAge() {
		return age;
	}

	public void setAge(int age) {
		this.age = age;
	}

	public GraduationStatus getGraduated() {
		return graduated;
	}

	public void setGraduated(GraduationStatus graduated) {
		this.graduated = graduated;
	}

	public Profession getProfession() {
		return profession;
	}

	public void setProfession(Profession profession) {
		this.profession = profession;
	}

	public int getWorkExperience() {
		return workExperience;
	}

	public void setWorkExperience(int workExperience) {
		this.workExperience = workExperience;
	}

	public SpendingScore getSpendingScore() {
		return spendingScore;
	}

	public void setSpendingScore(SpendingScore spendingScore) {
		this.spendingScore = spendingScore;
	}

	public int getFamilySize() {
		return familySize;
	}

	public void setFamilySize(int familySize) {
		this.familySize = familySize;
	}

	public Var1Category getVar1() {
		return var1;
	}

	public void setVar1(Var1Category var1) {
		this.var1 = var1;
	}

	public Segmentation getSegmentation() {
		return segmentation;
	}

	public void setSegmentation(Segmentation segmentation) {
		this.segmentation = segmentation;
	}
}