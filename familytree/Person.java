/**
 * 
 */
package familytree;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * @author Daniel Moulton
 * @date August 31, 2015
 *	
 *	A class representing a Person within the family tree.
 */

public class Person {
	
	//initial class variables
	private String name;
	private ArrayList<String> parentList = new ArrayList<String>();
	private ArrayList<String> childList = new ArrayList<String>();
	private ArrayList<String> spouseList = new ArrayList<String>();
	private ArrayList<String> siblingList = new ArrayList<String>();
	
	//constructor only takes the name of the person
	public Person (String name) {
		this.name = name;
	}
	
	//returns the name
	public String getName() {
		return name;
	}
	
	//adds a parent to the arraylist
	public void addParent(String parent) {
		if (parentList.size() >= 2) {
			System.out.println("Error: Too many parents");
		} else {
			parentList.add(parent);
		}
	}
	
	//checks if the given person is a parent of the Person object
	public boolean checkParent(String parent) {
		if (parentList.contains(parent)) {
			return true;
		}
		return false;
	}
	
	//returns the arraylist of the parents
	public ArrayList<String> getParents() {
		if (parentList.size() <= 0) {
			return null;
		}
		return parentList;
	}
	
	//adds a child to the arraylist
	public void addChild(String child) {
		childList.add(child);
	}
	
	//returns the arraylist of the children
	public ArrayList<String> getChildren() {
		if (childList.size() <= 0) {
			return null;
		}
		return childList;
	}
	
	//adds a spouse to the arraylist
	public void addSpouse(String spouse) {
		spouseList.add(spouse);
	}
	
	//returns the arraylist of marriages
	public ArrayList<String> getMarry() {
		if (spouseList.size() <= 0) {
			return null;
		}
		return spouseList;
	}
	
	//returns true if the given person is already a spouse of this person
	public boolean checkMarry(String person) {
		Iterator<String> it = spouseList.iterator();
		while (it.hasNext()) {
			if (person.equals(it.next())) {
				return true;
			}
		}
		
		return false;
	}
	
	//adds a sibling to the arraylist
	public void addSibling(String sibling) {
			siblingList.add(sibling);
	}
	
	//returns an ArrayList of all the siblings of this Person object
	public ArrayList<String> getSiblings() {
		if (siblingList.size() <= 0) {
			return null;
		}
		return siblingList;
	}
	
	//checks whether the given person is a sibling of the Person object
	public boolean checkSibling(String sibling) {
		if (siblingList.contains(sibling)) {
			return true;
		}
		return false;
	}
}
