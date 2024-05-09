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

package es.deusto.ingenieria.intelligensystems.customersegmentation.gui;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.net.URI;
import java.util.Arrays;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import es.deusto.ingenieria.intelligensystems.customersegmentation.controller.Controller;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Gender;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.GraduationStatus;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.MaritalStatus;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Profession;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Segmentation;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.SpendingScore;
import es.deusto.ingenieria.intelligensystems.customersegmentation.domain.Customer.Var1Category;

public class MainWindow extends JFrame {

	private Controller controller;

	private static final long serialVersionUID = 1L;

	private JTextField jTextAge = new JTextField();
	private JComboBox<Gender> jComboGender = new JComboBox<>();
	private JComboBox<MaritalStatus> jComboMaritalStatus = new JComboBox<>();
	private JTextField jTextWorkExperience = new JTextField();
	private JComboBox<GraduationStatus> jComboGraduated = new JComboBox<>();
	private JComboBox<Profession> jComboProfession = new JComboBox<>();
	private JComboBox<SpendingScore> jComboSpendingScore = new JComboBox<>();
	private JTextField jTextFamilySize = new JTextField();
	private JComboBox<Var1Category> jComboVar1 = new JComboBox<>();

	private JButton jBtnPredict = new JButton("Predict");

	public MainWindow(Controller controller) {
		this.controller = controller;

		JPanel panelMain = new JPanel();
		panelMain.setBorder(new TitledBorder("Customer data"));
		panelMain.setLayout(new GridBagLayout());

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.anchor = GridBagConstraints.WEST;
		gbc.insets = new Insets(2, 2, 2, 2);

		List<String> labels = Arrays.asList("Age:", "Gender:", "Marital Status:", "Work Experience:", "Graduated:",
				"Profession:", "Spending Score:", "Family Size:", "Var1 Category:");
		// Labels
		gbc.gridx = 0; // Column 0

		for (int i = 0; i < labels.size(); i++) {
			gbc.gridy = i; // Row 'i'
			panelMain.add(new JLabel(labels.get(i)), gbc);
		}

		// Age
		gbc.gridx = 1; // Column
		gbc.gridy = 0; // Row
		jTextAge.setHorizontalAlignment(JLabel.RIGHT);
		panelMain.add(jTextAge, gbc);

		// Gender
		jComboGender.addItem(null);
		Arrays.asList(Gender.values()).forEach(s -> jComboGender.addItem(s));

		gbc.gridy = 1;
		panelMain.add(jComboGender, gbc);

		// Marital Status
		jComboMaritalStatus.addItem(null);
		Arrays.asList(MaritalStatus.values()).forEach(c -> jComboMaritalStatus.addItem(c));

		gbc.gridy = 2;
		panelMain.add(jComboMaritalStatus, gbc);

		// Work Experience
		gbc.gridy = 3;
		jTextWorkExperience.setHorizontalAlignment(JLabel.RIGHT);
		panelMain.add(jTextWorkExperience, gbc);

		// Graduated
		jComboGraduated.addItem(null);
		Arrays.asList(GraduationStatus.values()).forEach(c -> jComboGraduated.addItem(c));

		gbc.gridy = 4;
		panelMain.add(jComboGraduated, gbc);

		// Profession
		jComboProfession.addItem(null);
		Arrays.asList(Profession.values()).forEach(c -> jComboProfession.addItem(c));

		gbc.gridy = 5;
		panelMain.add(jComboProfession, gbc);

		// Spending Score
		jComboSpendingScore.addItem(null);
		Arrays.asList(SpendingScore.values()).forEach(c -> jComboSpendingScore.addItem(c));

		gbc.gridy = 6;
		panelMain.add(jComboSpendingScore, gbc);

		// Family Size
		gbc.gridy = 7;
		jTextFamilySize.setHorizontalAlignment(JLabel.RIGHT);
		panelMain.add(jTextFamilySize, gbc);

		// Var1 Category
		jComboVar1.addItem(null);
		Arrays.asList(Var1Category.values()).forEach(c -> jComboVar1.addItem(c));

		gbc.gridy = 8;
		panelMain.add(jComboVar1, gbc);

		// Buttom Predict
		jBtnPredict.addActionListener(e -> {
			predict();
		});

		JPanel panelButtom = new JPanel(new BorderLayout(10, 10));
		panelButtom.add(jBtnPredict, BorderLayout.CENTER);

		JLabel jlabelCredits = new JLabel(
				"<html><a href=\"https://www.flaticon.com/authors/freepik\">Icons created by Freepik - Flaticon</a></html>");
		jlabelCredits.setHorizontalAlignment(JLabel.RIGHT);
		jlabelCredits.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		jlabelCredits.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				try {
					Desktop.getDesktop().browse(new URI("https://www.flaticon.com/authors/freepik"));
				} catch (Exception ex) {
					System.err.println("browser cannot be opened: https://www.flaticon.com/authors/freepik!");
				}
			}
		});

		panelButtom.add(jlabelCredits, BorderLayout.SOUTH);

		int maxFieldWidth = 200;
		Dimension fieldDimension = new Dimension(maxFieldWidth, jTextAge.getPreferredSize().height);

		jTextAge.setPreferredSize(fieldDimension);
		jComboGender.setPreferredSize(fieldDimension);
		jComboMaritalStatus.setPreferredSize(fieldDimension);
		jTextWorkExperience.setPreferredSize(fieldDimension);
		jComboGraduated.setPreferredSize(fieldDimension);
		jComboProfession.setPreferredSize(fieldDimension);
		jComboSpendingScore.setPreferredSize(fieldDimension);
		jTextFamilySize.setPreferredSize(fieldDimension);
		jComboVar1.setPreferredSize(fieldDimension);

		getContentPane().add(panelMain, BorderLayout.CENTER);
		getContentPane().add(panelButtom, BorderLayout.SOUTH);

		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setTitle("Customer Segmentation");
		setIconImage(new ImageIcon("resources/images/logo.png").getImage());
		setSize(380, 400);
		setLocationRelativeTo(null);
		setVisible(true);
	}

	private void predict() {
		boolean error = false;
		StringBuilder errorMessage = new StringBuilder();

		// Age
		if (jTextAge.getText().trim().isEmpty()) {
			error = true;
			errorMessage.append("- Age is required.\n");
		} else {
			try {
				Integer.parseInt(jTextAge.getText().trim());
			} catch (NumberFormatException ex) {
				error = true;
				errorMessage.append("- Age must be a valid integer.\n");
			}
		}

		// Work Experience
		if (jTextWorkExperience.getText().trim().isEmpty()) {
			error = true;
			errorMessage.append("- Work Experience is required.\n");
		} else {
			try {
				Integer.parseInt(jTextWorkExperience.getText().trim());
			} catch (NumberFormatException ex) {
				error = true;
				errorMessage.append("- Work Experience must be a valid integer.\n");
			}
		}

		// Family Size
		if (jTextFamilySize.getText().trim().isEmpty()) {
			error = true;
			errorMessage.append("- Family Size is required.\n");
		} else {
			try {
				Integer.parseInt(jTextFamilySize.getText().trim());
			} catch (NumberFormatException ex) {
				error = true;
				errorMessage.append("- Family Size must be a valid integer.\n");
			}
		}

		// Gender
		if (jComboGender.getSelectedIndex() == -1) {
			error = true;
			errorMessage.append("- Gender is required.\n");
		}

		// Marital Status
		if (jComboMaritalStatus.getSelectedIndex() == -1) {
			error = true;
			errorMessage.append("- Marital Status is required.\n");
		}

		// Graduated
		if (jComboGraduated.getSelectedIndex() == -1) {
			error = true;
			errorMessage.append("- Graduation Status is required.\n");
		}

		// Profession
		if (jComboProfession.getSelectedIndex() == -1) {
			error = true;
			errorMessage.append("- Profession is required.\n");
		}

		// Spending Score
		if (jComboSpendingScore.getSelectedIndex() == -1) {
			error = true;
			errorMessage.append("- Spending Score is required.\n");
		}

		// Var1 Category
		if (jComboVar1.getSelectedIndex() == -1) {
			error = true;
			errorMessage.append("- Var1 Category is required.\n");
		}

		if (error) {
			JOptionPane.showMessageDialog(null, errorMessage.toString(),
					"Input Error", JOptionPane.ERROR_MESSAGE, new ImageIcon("resources/images/error.png"));
		} else {
			// Get values from GUI components
	        Gender gender = (Gender) jComboGender.getSelectedItem();
	        MaritalStatus everMarried = (MaritalStatus) jComboMaritalStatus.getSelectedItem();
	        int age = Integer.parseInt(jTextAge.getText().trim());
	        GraduationStatus graduated = (GraduationStatus) jComboGraduated.getSelectedItem();
	        Profession profession = (Profession) jComboProfession.getSelectedItem();
	        int workExperience = Integer.parseInt(jTextWorkExperience.getText().trim());
	        SpendingScore spendingScore = (SpendingScore) jComboSpendingScore.getSelectedItem();
	        int familySize = Integer.parseInt(jTextFamilySize.getText().trim());
	        Var1Category var1 = (Var1Category) jComboVar1.getSelectedItem();

	        // Call controller method
	        Segmentation result = controller.predict(gender, everMarried, age, graduated, profession, workExperience, spendingScore, familySize, var1);

	        if (result != null) {
	        	JOptionPane.showMessageDialog(null, "After analyzing the customer's information, the associated category is \"" + result + "\".", 
	        			"Prediction Result", JOptionPane.INFORMATION_MESSAGE, new ImageIcon("resources/images/" + result + ".png"));
	        } else {
				JOptionPane.showMessageDialog(null, "After analyzing the customer's information, we cannot associate it with any of the categories.",
						"Prediction Result", JOptionPane.ERROR_MESSAGE, new ImageIcon("resources/images/error.png"));        	
	        }
		}
	}
}