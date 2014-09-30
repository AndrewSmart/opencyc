/* $Id: CancelManager.java 138070 2012-01-10 19:46:08Z sbrown $
 *
 * Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */

package org.opencyc.util;

//// Internal Imports

//// External Imports
import java.util.*;

/**
 * <P>CancelManager is designed to...
 *
 * <P>Copyright (c) 2004 - 2006 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author zelal
 * @date October 10, 2005, 4:03 PM
 * @version $Id: CancelManager.java 138070 2012-01-10 19:46:08Z sbrown $
 */
public class CancelManager {
  
  //// Constructors
  
  /** Creates a new instance of CancelManager. */
  public CancelManager() {
  }
  
  //// Public Area
  public static synchronized CancelManager getInstance() {
    if (me == null) {
      me = new CancelManager();
    }
    return me;
  }
  
  
  public synchronized void register(Cancelable current, Cancelable parent) {
    if (parent == null) {
      TreeNode currentNode = findNodeInTaskTrees(current);
      if (currentNode instanceof TreeNode) {
        TreeNode currentParent = currentNode.getParent();
        if (currentParent instanceof TreeNode) {
          Tree.removeFromParentChildren(currentNode);
        } else {
          removeFromTaskTrees(currentNode);
        }
        addToTaskTrees(new Tree(current, currentNode.getChildren(), null));
      } else {
        addToTaskTrees(new Tree(current, new ArrayList(), null));
      }
    } else {
      TreeNode parentNode = findNodeInTaskTrees(parent);
      TreeNode currentNode = findNodeInTaskTrees(current);
      Tree childNode;
      if (currentNode instanceof TreeNode) {
        childNode = new Tree(current, currentNode.getChildren(), parentNode);
        TreeNode currentParent = currentNode.getParent();
        if (currentParent instanceof TreeNode) {
          Tree.removeFromParentChildren(currentNode);
        } else {
          removeFromTaskTrees(currentNode);
        }
      } else {          
        childNode = new Tree(current, new ArrayList(), parentNode);
      }
      if (parentNode instanceof TreeNode) {
        parentNode.children.add(childNode);
      } else {
        addToTaskTrees(childNode);
      }
    }
  }
  
  public synchronized void unregister(Cancelable current) {
    TreeNode currentNode = findNodeInTaskTrees(current);    
    if (currentNode instanceof TreeNode) {
      List currentChildren = currentNode.getChildren();
      TreeNode currentParent = currentNode.getParent();
      for (Iterator iter = currentChildren.iterator(); iter.hasNext(); ) {
        Tree child = (Tree)iter.next();
        child.getRoot().setParent(currentParent);
      }
      if (currentParent instanceof TreeNode) { 
        Tree.removeFromParentChildren(currentNode);
        currentParent.getChildren().addAll(currentChildren);        
      } else {
        removeFromTaskTrees(currentNode);
        for (Iterator iter = currentChildren.iterator(); iter.hasNext(); ) {
          addToTaskTrees((Tree)iter.next());
        }
      }
    }
  }

 
  public synchronized void cancel(Cancelable root) {
    root.cancelTask();
    TreeNode rootNode = findNodeInTaskTrees(root);
    if (rootNode instanceof TreeNode) {
      List children = rootNode.getChildren();
      TreeNode parentNode = rootNode.getParent();
      for (Iterator iter = children.iterator(); iter.hasNext(); ) {
        TreeNode nextNode = ((Tree)(iter.next())).getRoot();
        cancelRecursive(nextNode);
      }
      if (parentNode instanceof TreeNode) {        
        Tree.removeFromParentChildren(rootNode);
      } else {
        removeFromTaskTrees(rootNode);
      }
    }
  }
  
  private void cancelRecursive(TreeNode root) {
    ((Cancelable)root.getDatum()).cancelTask();
    //Tree.removeFromParentChildren(root);
    List children = root.getChildren();
    for (Iterator iter = children.iterator(); iter.hasNext(); ) {
        TreeNode nextNode = ((Tree)(iter.next())).getRoot();
        cancelRecursive(nextNode);
    }
    //Tree.removeFromParentChildren(root);   
  }
  //// Protected Area
  
  //// Private Area
  private static class TreeNode {
    private Object datum;
    private List children;
    private TreeNode parent;
    
    TreeNode(Object datum, List children, TreeNode parent) {
      this.datum = datum;
      this.children = children;
      this.parent = parent;
    }
    
    List getChildren() {
      return children;
    }
    
    Object getDatum() {
      return datum;
    }
    
    TreeNode getParent() {
      return parent;
    }
    
    void setParent(TreeNode newParent) {
      this.parent = newParent;
    }
    
  }
  
  private static class Tree {
    private TreeNode root;
    
    TreeNode getRoot() {
      return root;
    }
    
    Tree(Cancelable rootDatum, List rootChildren, TreeNode rootParent) {
      root = new TreeNode(rootDatum, rootChildren, rootParent);
    }
    
    static TreeNode findNodeOneLevel(TreeNode node, Object nodeDatum) {
      if (nodeDatum.equals(node.getDatum())) {
        return node;
      } 
      return null;
   }
    
   static TreeNode findNode(Tree tree, Object nodeDatum) {
     if (tree == null) { 
       return null;
     } 
     TreeNode node = findNodeOneLevel(tree.getRoot(), nodeDatum);
     TreeNode nextNode = tree.getRoot();
     if (!(node instanceof TreeNode)) {
       List children = nextNode.getChildren();
       for (Iterator iter = children.iterator(); iter.hasNext(); ) {
         Tree nextTree = (Tree)iter.next();
         node = findNode(nextTree, nodeDatum);
         if (!(node instanceof TreeNode)) {
           nextNode = nextTree.getRoot();
         }
       }
     }
     return node;
   }
   
   
   private static void removeFromTrees(TreeNode treeNode, List trees) {
    boolean found = false;
    // without this initialization, I get a compilation error
    // that says subTree may not have been initialized
    Tree subTree = new Tree(null, null, null);
    for (Iterator iter = trees.iterator(); iter.hasNext(); ) {
      subTree = (Tree)iter.next();
      if (treeNode.equals(subTree.getRoot())) {
        found = true;
        break;
      }
    }
    if (found == true) {  // || (subTree instanceof Tree)) {
      trees.remove(subTree);
    }
   }
   
   private static void removeFromParentChildren(TreeNode treeNode) {
     removeFromTrees(treeNode, treeNode.getParent().getChildren());
   }
   
   static void printTree(Tree tree) {
     if (tree == null) { 
       ; //System.out.println(tree);
     } else {
       printTreeRecursive(tree, "");
     }
   }
   
   static void printTreeRecursive(Tree tree, String indent) {       
     TreeNode root = tree.getRoot();
     Object rootDatum = root.getDatum();
     System.out.println(indent + rootDatum);
     List children = root.getChildren();
     for (Iterator iter = children.iterator(); iter.hasNext(); ) {
       Tree nextTree = (Tree)iter.next();
       printTreeRecursive(nextTree, indent+" ");
     }
   }
   
 }
  
  private TreeNode findNodeInTaskTrees(Object nodeDatum) {
    if (taskTrees == null) {
      return null;
    }
    TreeNode node = null;
    for (Iterator iter = taskTrees.iterator(); iter.hasNext(); ) {
      Tree nextTree = (Tree)iter.next();
      node = Tree.findNode(nextTree, nodeDatum);
      if (node instanceof TreeNode) {
        return node;
      }
    }
    return node;
  }
  
  
  private List getTaskTrees() {
    return taskTrees;
  }
  
  private void setTaskTrees(List trees) {
    taskTrees = trees;
  }
  
  private void addToTaskTrees(Tree tree) {
    taskTrees.add(tree);
  }
  
  // removes the tree whose root equals treeNode from taskTrees
  private void removeFromTaskTrees(TreeNode treeNode) {
    Tree.removeFromTrees(treeNode, taskTrees);
  }
  
  private void printTaskTrees() {
    System.out.println("Task Trees:");
    for (Iterator iter = taskTrees.iterator(); iter.hasNext(); ) {
      Tree nextTree = (Tree)iter.next();
      Tree.printTree(nextTree);
    }
  }
  
  
  //// Internal Rep
  private List taskTrees = new ArrayList();
  private static CancelManager me;
  //// Main
  
  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    Cancelable task1 = new DefaultCancelable("A");
    Cancelable task2 = new DefaultCancelable("B");
    Cancelable task3 = new DefaultCancelable("C");
    me = getInstance();
    me.register(task1, null);
    me.register(task2, task1);
    me.register(task3, task2);
    me.printTaskTrees();
    me.cancel(task1);
    me.printTaskTrees();    
    
    System.out.println();
    
    Cancelable task4 = new DefaultCancelable("D");
    Cancelable task5 = new DefaultCancelable("E");
    Cancelable task6 = new DefaultCancelable("F");
    Cancelable task7 = new DefaultCancelable("G");
    Cancelable task8 = new DefaultCancelable("H");
    Cancelable task9 = new DefaultCancelable("I");
    me.register(task4, null);
    me.register(task5, task4);
    me.register(task6, task5);
    me.register(task7, task4);
    me.register(task8, task7);
    me.register(task9, task7);
    me.printTaskTrees();  
    me.cancel(task7);
    me.printTaskTrees();  
    
    System.out.println();
    
    Cancelable task10 = new DefaultCancelable("J");
    Cancelable task11 = new DefaultCancelable("K");
    me.register(task10, null);
    me.register(task11, task10);
    me.printTaskTrees();  
    me.cancel(task11);    
    me.printTaskTrees();  
    me.cancel(task10);    
    me.printTaskTrees();      
    
    me.register(task1, null);
    me.register(task2, task1);
    me.register(task3, task2);
    me.printTaskTrees();
    me.unregister(task1);
    me.printTaskTrees();    
   
    me.register(task5, task2);
    me.printTaskTrees();    
    
    me.register(task2, task4);
    me.printTaskTrees();    
  }
  
}
