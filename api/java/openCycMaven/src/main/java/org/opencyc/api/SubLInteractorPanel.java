/*
 * SubLInteractorPanel.java
 *
 * Created on October 8, 2008, 1:27 PM
 */
package org.opencyc.api;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.lang.Integer;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import static org.opencyc.cycobject.DefaultCycObject.cyclify;
import static org.opencyc.cycobject.DefaultCycObject.isCycLObject;

/**
 *
 * @author  baxter
 */
public class SubLInteractorPanel extends javax.swing.JPanel {

  final private SubLInteractor interactor;
  private boolean isRunning = false;
  static private final List<String> TIMEOUT_OPTION_STRINGS =
      Arrays.asList("None", "1 second", "5 seconds", "30 seconds", "1 minute", "5 minutes");
  static private final List<Integer> TIMEOUT_OPTION_VALUES =
      Arrays.asList(0, 1000, 5000, 30000, 60000, 300000);

  /** Creates new form SubLInteractorPanel */
  public SubLInteractorPanel(final SubLInteractor interactor) {
    this.interactor = interactor;
    initComponents();
    setTimeoutMsecsFromTimeoutComboBox();
  }

  private ComboBoxModel makeTimeoutComboBoxModel() {
    return new DefaultComboBoxModel(TIMEOUT_OPTION_STRINGS.toArray());
  }

  private void displayResult(final List result) {
    final StringBuilder resultStringBuilder = new StringBuilder();
    if (result == null) {
      resultStringBuilder.append(getResultString(result));
    } else if (result.size() == 1) {
      resultStringBuilder.append(getResultString(result.get(0)));
    } else {
      for (int i = 0; i < result.size(); i++) {
        final Object resultObj = result.get(i);
        resultStringBuilder.append("Value " + i + ": " + getResultString(resultObj) + '\n');
      }
    }

    displayOutput(Color.BLACK, resultStringBuilder.toString());
  }

  private void displayException(final Exception e) {
    ByteArrayOutputStream stream = new ByteArrayOutputStream();
    PrintWriter pw = new PrintWriter(stream);
    e.printStackTrace(pw);
    pw.flush();
    displayOutput(Color.RED, stream.toString());
  }

  /** @note color not used. We'd like to use it for just output, not the whole field. */
  private void displayOutput(final Color color, final String output) {
    SwingUtilities.invokeLater(new Runnable() {

      public void run() {
        outputField.append("\n==============================================\n");
        outputField.append("Input: " + inputField.getText());
        outputField.append("\n\nResult:\n");
        outputField.append(output);
        setGUIToIdleMode();
      }
    });
  }

  private String getResultString(final Object resultObj) {
    return (isCycLObject(resultObj)) ? cyclify(resultObj) : (resultObj == null) ? "null" : resultObj.toString();
  }

  private void setTimeoutMsecsFromTimeoutComboBox() {
    final String selectedString = (String) timeoutComboBox.getSelectedItem();
    final int value = TIMEOUT_OPTION_VALUES.get(TIMEOUT_OPTION_STRINGS.indexOf(selectedString));
    interactor.setTimeoutMsecs(value);
  }

  private void setGUIToRunningMode() {
    inputField.setEditable(false);
    inputField.setForeground(Color.DARK_GRAY);
    evalCancelButton.setText("Cancel");
    isRunning = true;
  }

  private void setGUIToIdleMode() {
    inputField.setEditable(true);
    inputField.setForeground(Color.BLACK);
    evalCancelButton.setText("Eval");
    isRunning = false;
  }

//// Main
  public static void main(String[] args) {
    try {
      CycAccess cycAccess = CycAccess.getNewCycAccessInteractively();
      if (cycAccess == null) {
        System.exit(-1);
      }
      SubLInteractor interactor = new SubLInteractor(cycAccess);
      final JFrame frame = new JFrame();
      frame.setTitle("SubL Interactor (" + cycAccess.getHostName() + ":" + 
          cycAccess.getBasePort() + ")");
      frame.setMinimumSize(new Dimension(300, 300));
      frame.setPreferredSize(new Dimension(400, 600));
      SubLInteractorPanel interactorPanel = new SubLInteractorPanel(interactor);
      interactorPanel.addComponentListener(new ComponentAdapter() {

        public void componentHidden(ComponentEvent e) {
          System.exit(0);
        }
      });
      frame.getContentPane().add(interactorPanel);
      frame.addWindowListener(new WindowAdapter() {

        public void windowClosing(WindowEvent e) {
          System.exit(0);
        }
      });
      frame.pack();
      frame.setVisible(true);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {
    java.awt.GridBagConstraints gridBagConstraints;

    mainTitleLabel = new javax.swing.JLabel();
    hostInfoLabel = new javax.swing.JLabel();
    buttonPanel = new javax.swing.JPanel();
    evalCancelButton = new javax.swing.JButton();
    clearButton = new javax.swing.JButton();
    timeoutLabel = new javax.swing.JLabel();
    timeoutComboBox = new javax.swing.JComboBox();
    quitButton = new javax.swing.JButton();
    jSplitPane1 = new javax.swing.JSplitPane();
    inputFieldScrollPane = new javax.swing.JScrollPane();
    inputField = new javax.swing.JTextArea();
    resultsPanel = new javax.swing.JPanel();
    resultsLabel = new javax.swing.JLabel();
    outputFieldScrollPane = new javax.swing.JScrollPane();
    outputField = new javax.swing.JTextArea();

    setMinimumSize(new java.awt.Dimension(400, 600));
    setLayout(new java.awt.GridBagLayout());

    mainTitleLabel.setFont(mainTitleLabel.getFont().deriveFont(mainTitleLabel.getFont().getStyle() | java.awt.Font.BOLD, mainTitleLabel.getFont().getSize()+4));
    mainTitleLabel.setText("SubL Interactor");
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    add(mainTitleLabel, gridBagConstraints);

    hostInfoLabel.setText(interactor.getCycAccess().toString());
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    add(hostInfoLabel, gridBagConstraints);

    evalCancelButton.setText("Eval");
    evalCancelButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        evalCancelButtonActionPerformed(evt);
      }
    });
    buttonPanel.add(evalCancelButton);

    clearButton.setText("Clear");
    clearButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        clearButtonActionPerformed(evt);
      }
    });
    buttonPanel.add(clearButton);

    timeoutLabel.setText("timeout:");
    buttonPanel.add(timeoutLabel);

    timeoutComboBox.setModel(makeTimeoutComboBoxModel());
    timeoutComboBox.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        timeoutComboBoxActionPerformed(evt);
      }
    });
    buttonPanel.add(timeoutComboBox);

    quitButton.setText("Quit");
    quitButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        quitButtonActionPerformed(evt);
      }
    });
    buttonPanel.add(quitButton);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    add(buttonPanel, gridBagConstraints);

    jSplitPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
    jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
    jSplitPane1.setResizeWeight(0.5);
    jSplitPane1.setOneTouchExpandable(true);

    inputField.setFont(inputField.getFont().deriveFont(inputField.getFont().getSize()+3f));
    inputField.setLineWrap(true);
    inputField.setRows(5);
    inputField.setWrapStyleWord(true);
    inputFieldScrollPane.setViewportView(inputField);

    jSplitPane1.setTopComponent(inputFieldScrollPane);

    resultsPanel.setLayout(new java.awt.GridBagLayout());

    resultsLabel.setText("Results");
    resultsLabel.setAlignmentY(0.0F);
    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.gridwidth = java.awt.GridBagConstraints.REMAINDER;
    resultsPanel.add(resultsLabel, gridBagConstraints);

    outputField.setColumns(20);
    outputField.setEditable(false);
    outputField.setLineWrap(true);
    outputField.setRows(5);
    outputField.setWrapStyleWord(true);
    outputFieldScrollPane.setViewportView(outputField);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    resultsPanel.add(outputFieldScrollPane, gridBagConstraints);

    jSplitPane1.setRightComponent(resultsPanel);

    gridBagConstraints = new java.awt.GridBagConstraints();
    gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
    gridBagConstraints.weightx = 1.0;
    gridBagConstraints.weighty = 1.0;
    gridBagConstraints.insets = new java.awt.Insets(2, 10, 2, 10);
    add(jSplitPane1, gridBagConstraints);
  }// </editor-fold>//GEN-END:initComponents

private void evalCancelButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_evalCancelButtonActionPerformed
  if (isRunning == true) {
    try {
      interactor.cancelLastCommand();
    } catch (Exception e) {
    }
    setGUIToIdleMode();
  } else {
    setGUIToRunningMode();
    new Thread() {

      public void run() {
        try {
          final List result = interactor.submitSubL(inputField.getText());
          displayResult(result);
        } catch (Exception ex) {
          Logger.getLogger(SubLInteractorPanel.class.getName()).log(Level.SEVERE, null, ex);
          displayException(ex);
        }
      }
    }.start();
  }
}//GEN-LAST:event_evalCancelButtonActionPerformed

private void clearButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_clearButtonActionPerformed
  inputField.setText("");
}//GEN-LAST:event_clearButtonActionPerformed

private void timeoutComboBoxActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_timeoutComboBoxActionPerformed
  setTimeoutMsecsFromTimeoutComboBox();
}//GEN-LAST:event_timeoutComboBoxActionPerformed

private void quitButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_quitButtonActionPerformed
  setVisible(false);
  interactor.quit();
}//GEN-LAST:event_quitButtonActionPerformed
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel buttonPanel;
  private javax.swing.JButton clearButton;
  private javax.swing.JButton evalCancelButton;
  private javax.swing.JLabel hostInfoLabel;
  private javax.swing.JTextArea inputField;
  private javax.swing.JScrollPane inputFieldScrollPane;
  private javax.swing.JSplitPane jSplitPane1;
  private javax.swing.JLabel mainTitleLabel;
  private javax.swing.JTextArea outputField;
  private javax.swing.JScrollPane outputFieldScrollPane;
  private javax.swing.JButton quitButton;
  private javax.swing.JLabel resultsLabel;
  private javax.swing.JPanel resultsPanel;
  private javax.swing.JComboBox timeoutComboBox;
  private javax.swing.JLabel timeoutLabel;
  // End of variables declaration//GEN-END:variables
}
