package org.opencyc.util;

/**
 * Provides a gui List component.
 *
 * @version $Id: ListBox.java 138070 2012-01-10 19:46:08Z sbrown $
 * @author Stephen L. Reed
 *
 * <p>Copyright 2001 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://www.sourceforge.net/projects/opencyc">OpenCyc at SourceForge</a>
 * <p>
 * THIS SOFTWARE AND KNOWLEDGE BASE CONTENT ARE PROVIDED ``AS IS'' AND
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENCYC
 * ORGANIZATION OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE AND KNOWLEDGE
 * BASE CONTENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ListBox extends JPanel {
    protected Action addAction, removeAction, updateAction;
    protected JList list = new JList();
    protected JTextField field = new JTextField();
    protected JButton addButton = new JButton("Add");
    protected JButton removeButton = new JButton("Remove");
    protected Object lastSelected = null;

    /**
     * Constructs a new ListBox object given a title and row count.
     *
     * @param title the list box title
     * @param rowCount the number of list items in the list box.
     */
    public ListBox(String title, int rowCount) {
        this(null, null, null, title, rowCount);
    }

    /**
     * Constructs a ListBox initialized with the given actions, title and row count.
     *
     * @param addAction the action to be taken when an item is added to the list box
     * @param removeAction the action to be taken when an item is removed from the list box
     * @param updateAction the action to be taken when an item is modified in the list box
     * @param title the list box title
     * @param rowCount the number of list items in the list box.
     */
    public ListBox(Action addAction,
                   Action removeAction,
                   Action updateAction,
                   String title,
                   int rowCount){
        super();
        this.addAction = addAction;
        this.removeAction = removeAction;

        setLayout(new BorderLayout());
        removeButton.setEnabled(false);
        list.setVisibleRowCount(rowCount);
        list.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent ev) {
                int idx = ev.getFirstIndex();

                if (idx == -1)
                    lastSelected = null;
                else
                    lastSelected =
                        ((JList) ev.getSource()).getModel().getElementAt(idx);

                if (ev.getValueIsAdjusting())
                    return;

                if (((JList) ev.getSource()).getSelectedIndex() == -1)
                    removeButton.setEnabled(false);
                else
                    removeButton.setEnabled(true);
            }
        });

        setLayout(new BorderLayout());
        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        JPanel gridPanel = new JPanel();
        gridPanel.setLayout(new GridLayout(2, 2));

        listPanel.add(new JLabel(title), BorderLayout.NORTH);
        listPanel.add(new JScrollPane(list,
                                  JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                  JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED),
                      BorderLayout.CENTER);

        gridPanel.add(addButton);
        gridPanel.add(field);

        gridPanel.add(removeButton);
        add(listPanel, BorderLayout.CENTER);
        add(gridPanel, BorderLayout.SOUTH);

        if (addAction != null)
            addButton.addActionListener(addAction);

        if (removeAction != null)
            removeButton.addActionListener(removeAction);

        // also update after add/remove
        if (updateAction != null) {
            addButton.addActionListener(updateAction);
            removeButton.addActionListener(updateAction);

            updateAction.actionPerformed(new ActionEvent(this, 0, null));
        }
    }

    // Change the action set
    public void setActions(Action add, Action remove, Action update) {
        if (addAction != null)
            addButton.removeActionListener(addAction);

        if (removeAction != null)
            removeButton.removeActionListener(removeAction);

        if (updateAction != null) {
            addButton.removeActionListener(updateAction);
            removeButton.removeActionListener(updateAction);
        }

        addAction = add;
        removeAction = remove;
        updateAction = update;

        if (addAction != null)
            addButton.addActionListener(addAction);

        if (removeAction != null)
            removeButton.addActionListener(removeAction);

        if (updateAction != null) {
            addButton.addActionListener(updateAction);
            removeButton.addActionListener(updateAction);

            updateAction.actionPerformed(new ActionEvent(this, 0, null));
        }
    }

    /**
     * Gets the selected value.
     */
    public Object getSelectedValue() {
        return lastSelected;
    }

    /**
     * Gets the list that the ListBox is modelling.
     */
    public JList getList() {
        return list;
    }

    /**
     * Gets the text field.
     */
    public JTextField getField() {
        return field;
    }
}
