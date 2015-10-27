/**
 * 
 */
package familytree;

import java.io.FileReader;
import java.io.IOException;
import java.io.BufferedReader;
import java.util.Scanner;
import java.util.Hashtable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Collections;
import java.util.TreeSet;
import java.util.Set;

/**
 * @author Daniel Moulton
 * @date   August 25, 2015
 * Family Tree Project
 * A Java implementation of the Organization of Programming Languages family tree project.
 * The program will accept input from a standard in text file input in a line by line format of
 * family tree relationships. It will represent this family tree in the form of a directed graph
 * with people objects and will accept queries at the end of the text file involving relationships
 * and ancestry within the family tree.
 * 
 * Possible queries are:
 * 	how closely related two people in the tree are
 * 	do 2 people have a certain relationship to each other
 * 	list all of a certain type of relation of a certain person
 *  
 * Possible relations are:
 * 	spouse, parent, sibling, ancestor, relative, unrelated
 */
public class FamilyTreeDriver {

	public static void main(String[] args) throws IOException {
		
		//Sets up initial variables and the input
		//Scanner s = new Scanner(System.in);
		//String filePath = s.next();
		String filePath = "testinput.txt";
		FileReader fReader = new FileReader(filePath);
		BufferedReader bReader = new BufferedReader(fReader);
		
		Hashtable<String, Person> personHash = new Hashtable<String, Person>();
		
		String in, name, query, parent1, parent2, relation, person;
		
		//Keeps reading line by line until the txt file is empty
		while ((in = bReader.readLine()) != null) {
			Scanner scan = new Scanner(in);
			query = scan.next();
			String result = "";
			
			//Adding children to the tree
			if (query.equals("E")) {
				parent1 = scan.next();
				if (!personHash.containsKey(parent1)) {
					personHash.put(parent1, new Person(parent1));
				}
				parent2 = scan.next();
				if (!personHash.containsKey(parent2)) {
					personHash.put(parent2, new Person(parent2));
				}
				name = scan.next();
				personHash.put(name, new Person(name));
				
				personHash = createPPC(parent1, parent2, name, personHash);
			}
			
			//List everyone who is the relation of the person
			else if (query.equals("W")) {
				System.out.println(in);
				relation = scan.next();
				person = scan.next();
				
				if (personHash.containsKey(person)) {
					//switch statement handling each relation possibility
					switch (relation) {
						case ("parent"):
							result = getParents(personHash, person);
							break;
						case ("ancestor"):
							result = getAncestors(personHash, person);
							break;
						case ("spouse"):
							result = getMarry(personHash, person);
							break;
						case ("relative"):
							result = getRelatives(personHash, person);
							break;
						case ("sibling"):
							result = getSiblings(personHash, person);
							break;
						case ("unrelated"):
							result = getUnrelated(personHash, person);
							break;
						default:
							result = "This is an invalid relation";
							break;
					}
				}
					
				String[] finalList = result.split("\\s+");
				for (int i=0; i<finalList.length; i++) {
					System.out.println(finalList[i]);
				}
				System.out.println();
			}
			
			//Check whether the given person is the relation of the other given person
			else if (query.equals("X")) {
				System.out.println(in);
				person = scan.next();
				relation = scan.next();
				name = scan.next();
				boolean last = false;
				
				if (personHash.containsKey(person) && personHash.containsKey(name)) {
					//switch statement handling each relation possibility
					switch (relation) {
						case ("parent"):
							if (personHash.get(person).checkParent(name)) {
								last = true;
							}
							break;
						case ("ancestor"):
							TreeSet<String> ancestorSet = new TreeSet<String>();
							ancestorSet = recAncestors(personHash, person);
							if (ancestorSet.contains(name)) {
								last = true;
							}
							break;
						case ("spouse"):
							if (personHash.get(person).checkMarry(name)) {
								last = true;
							}
							break;
						case ("relative"):
							String[] relativeArray;
							relativeArray = getRelatives(personHash, person).split("\\s+");
							for (int i=0; i<relativeArray.length; i++) {
								if (name.equals(relativeArray[i])) {
									last = true;
								}
							}
							break;
						case ("sibling"):
							if (personHash.get(person).checkSibling(name)) {
								last = true;
							}
							break;
						case ("unrelated"):
							String [] unrelatedArray;
							unrelatedArray = getUnrelated(personHash, person).split("\\s+");
							for (int j=0; j<unrelatedArray.length; j++) {
								if (name.equals(unrelatedArray[j])) {
									last = true;
								}
							}
							break;
						default:
							System.out.println("This is an invalid relation");
							continue;
					}
					
					if (last) {
						System.out.println("Yes");
					}
					else {
						System.out.println("No");
					}
				} else {
					System.out.println();
				}
				System.out.println();
			}
			
			//what is the closest relation the two people have with each other
			else if (query.equals("R")) {
				System.out.println(in);
				person = scan.next();
				name = scan.next();
				String finalRel = "Error";
				finalRel = rHandler(personHash, person, name);
				System.out.println(finalRel);
				System.out.println();
			}
			
			else {
				System.out.println("Invalid Query");
			}
			
		
			scan.close();
		}
		
		//Ending lines, wrap up all loose ends
		fReader.close();
		//s.close();

	}
	
	//Generates a relationship in the arraylist of the person objects between two parents and their child
	public static Hashtable<String, Person> createPPC(String parent1, String parent2, String child, 
							Hashtable<String, Person> personHash) {
		//marriage
		if (!(personHash.get(parent1).checkMarry(parent2))) {
			personHash.get(parent1).addSpouse(parent2);
		}
		if (!(personHash.get(parent2).checkMarry(parent2))) {
			personHash.get(parent2).addSpouse(parent1);
		}
		
		//add child
		personHash.get(parent1).addChild(child);
		personHash.get(parent2).addChild(child);
		
		//add parents
		personHash.get(child).addParent(parent1);
		personHash.get(child).addParent(parent2);
		
		//add siblings
		personHash = checkSiblings(parent1, parent2, child, personHash);
		
		return personHash;
	}
	
	//checks whether the given parent, parent, child relationship has generated any new siblings and stores them in the Person's class
	public static Hashtable<String, Person> checkSiblings(String parent1, String parent2, String child,
							Hashtable<String, Person> personHash) {
		ArrayList<String> pList1 = new ArrayList<String>();
		ArrayList<String> pList2 = new ArrayList<String>();
		pList1 = personHash.get(parent1).getChildren();
		pList2 = personHash.get(parent2).getChildren();
		ArrayList<String> commonList = new ArrayList<String>(pList1);
		commonList.retainAll(pList2);
		Iterator<String> it = commonList.iterator();
		while (it.hasNext()) {
			String str = it.next();
			personHash.get(child).addSibling(str);
			personHash.get(str).addSibling(child);
		}
		return personHash;
	}
	
	//gets the parents of the given person and returns them as a string separated by a new line
	public static String getParents(Hashtable<String, Person> personHash, String person) {
		ArrayList<String> parentList = new ArrayList<String>();
		String list = "";
		parentList = personHash.get(person).getParents();
		if (parentList != null) {
			Collections.sort(parentList);
			Iterator<String> it = parentList.iterator();
			while (it.hasNext()) {
				list += personHash.get(it.next()).getName() + " ";
			}
			return list;
		} else {
			return "";
		}
	}
	
	//gets all the ancestors of the given person and returns them in a string separated by new lines
	public static String getAncestors(Hashtable<String, Person> personHash, String person) {
		TreeSet<String> result = new TreeSet<String>();
		String finalList = "";
		result = recAncestors(personHash, person);
		Iterator<String> it = result.iterator();
		while (it.hasNext()) {
			finalList += it.next() + " ";
		}
		return finalList;
	}
	
	//returns a TreeSet of all the ancestors of the given person to get rid of duplicates and sort alphabetically
	//recursive function that utilizes the parentLists of each parent to find every ancestor
	public static TreeSet<String> recAncestors(Hashtable<String, Person> personHash, String person) {
		TreeSet<String> result = new TreeSet<String>();
		if (personHash.get(person).getParents() == null) {
			result.add(person);
			return result;
		}
		else {
			ArrayList<String> parentList = new ArrayList<String>();
			parentList = personHash.get(person).getParents();
			Iterator<String> it = parentList.iterator();
			while (it.hasNext()) {
				String nextPerson = it.next();
				result.add(nextPerson);
				result.addAll(recAncestors(personHash, nextPerson));
			}
		}
		return result;
	}
	
	//returns a string list of all the spouses of the given person
	public static String getMarry(Hashtable<String, Person> personHash, String person) {
		ArrayList<String> spouseList = new ArrayList<String>();
		String list = "";
		spouseList = personHash.get(person).getMarry();
		if (spouseList != null) {
			Collections.sort(spouseList);
			Iterator<String> it = spouseList.iterator();
			while (it.hasNext()) {
				list += personHash.get(it.next()).getName() + " ";
			}
			return list;
		} else {
			return "";
		}
	}
	
	//gets all the relatives of the given person and returns them in a string separated by new lines
	public static String getRelatives(Hashtable<String, Person> personHash, String person) {
		TreeSet<String> result = new TreeSet<String>();
		TreeSet<String> ancestorList = new TreeSet<String>();
		String finalList = "";
		ancestorList.addAll(recAncestors(personHash, person));
		Iterator<String> it = ancestorList.iterator();
		while (it.hasNext()) {
			String dude = it.next();
			result.add(dude);
			result.addAll(recChildren(personHash, dude));
		}
		it = result.iterator();
		while (it.hasNext()) {
			String nextPerson = it.next();
			finalList += nextPerson + " ";
		}
		return finalList;
	}
	
	//returns a TreeSet of all the children of the given person to get rid of duplicates and sort alphabetically
	//recursive function that utilizes the childLists of each child to find every child
	public static TreeSet<String> recChildren(Hashtable<String, Person> personHash, String person) {
		TreeSet<String> result = new TreeSet<String>();
		if (personHash.get(person).getChildren() == null) {
			return result;
		} else {
			ArrayList<String> childList = new ArrayList<String>();
			childList = personHash.get(person).getChildren();
			Iterator<String> it = childList.iterator();
			while (it.hasNext()) {
				String nextPerson = it.next();
				result.add(nextPerson);
				result.addAll(recChildren(personHash, nextPerson));
			}
		}
		return result;
	}
	
	//gets all the siblings of the given person and returns them in a string separated by new lines
	public static String getSiblings(Hashtable<String, Person> personHash, String person) {
		String finalList = "";
		TreeSet<String> siblingList = new TreeSet<String>();
		if (personHash.get(person).getSiblings() == null) {
			return "";
		}
		siblingList.addAll(personHash.get(person).getSiblings());
		Iterator<String> it = siblingList.iterator();
		while (it.hasNext()) {
			finalList += it.next() + " ";
		}
		return finalList;
	}
	
	//gets a TreeSet of every possible relation, and then compares it with a full list of the hashtable to figure out which members
	//are completely unrelated
	public static String getUnrelated(Hashtable<String, Person> personHash, String person) {
		String finalList = "";
		String[] personArray;
		TreeSet<String> compareList = new TreeSet<String>();
		
		//Relatives
		personArray = getRelatives(personHash, person).split("\\s+");
		for (int i=0; i<personArray.length; i++) {
			compareList.add(personArray[i]);
		}
		
		//Spouses
		personArray = getMarry(personHash, person).split("\\s+");
		for (int j=0; j<personArray.length; j++) {
			compareList.add(personArray[j]);
		}
		
		Set<String> personSet = personHash.keySet();
		Iterator<String> it = personSet.iterator();
		while (it.hasNext()) {
			String comparePerson = it.next();
			if (!compareList.contains(comparePerson)) {
				finalList += comparePerson + " ";
			}
		}
		
		return finalList;
	}
	
	//Handles the R query of the program, searching for the closest relation between two people
	public static String rHandler(Hashtable<String, Person> personHash, String person, String name) {
		String finalString = "unrelated";
		TreeSet<String> ancestorSet = new TreeSet<String>();
		if (personHash.containsKey(person) && personHash.containsKey(name)) {
			ancestorSet = recAncestors(personHash, person);
			String[] relativeArray;
			relativeArray = getRelatives(personHash, person).split("\\s+");
			
			//spouse
			if (personHash.get(person).checkMarry(name)) {
				finalString = "spouse";
			}
			
			//parent
			else if (personHash.get(person).checkParent(name)) {
				finalString = "parent";
			}
			
			//sibling
			else if (personHash.get(person).checkSibling(name)) {
				finalString = "sibling";
			}
			
			//ancestor
			else if (ancestorSet.contains(name)) {
				finalString = "ancestor";
			}
			
			//relative
			else {
				for (int i=0; i<relativeArray.length; i++) {
					if (name.equals(relativeArray[i])) {
						finalString = "relative";
					}
				}
			}
		}
		return finalString;
	}
}
