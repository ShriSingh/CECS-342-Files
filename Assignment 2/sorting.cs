/*
  Writing a sorting algorithm that can handle any data type 
  Then test the algorithm with test cases
    - sort numbers ascending by numerical value,
    - sort people alphabetically (lexicographically) by name, and to
    - sort people descending by age, where people of the same age should be sorted alphabetically (lexicographically).
*/

using System;
using System.Collections.Generic;

// Class that serves to function as a person
class Person {
    public string Name { 
        get; 
        set; 
    }
    public int Age { 
        get; 
        set; 
    }
}

class Program {
    //Functions as a bubble sort
    static void sort<T>(T[] array, Func<T, T, bool> comp){
        int len = array.Length;
        for (int i = 0; i < len - 1; i++){
            for (int j = 0; j < len - i - 1; j++){
                if (!comp(array[j], array[j + 1])){
                    T index = array[j];
                  array[j] = array[j + 1];
                  array[j + 1] = index;
                } } } }
    static void Main(string[] args){
      //establishes the sample data for the test
      Person[] names = {
          new Person { Name = "Hal", Age = 20 }, new Person { Name = "Susann", Age = 31 }, new Person { Name = "Dwight", Age = 19 }, new Person { Name = "Kassandra", Age = 21 },
          new Person { Name = "Lawrence", Age = 25 }, new Person { Name = "Cindy", Age = 22 }, new Person { Name = "Cory", Age = 27 }, new Person { Name = "Mac", Age = 19 },
          new Person { Name = "Romana", Age = 27 }, new Person { Name = "Doretha", Age = 32 }, new Person { Name = "Danna", Age = 20 }, new Person { Name = "Zara", Age = 23 },
          new Person { Name = "Rosalyn", Age = 26 }, new Person { Name = "Risa", Age = 24 }, new Person { Name = "Benny", Age = 28 }, new Person { Name = "Juan", Age = 33 },
          new Person { Name = "Natalie", Age = 25 }
      };
      double[] num = { 645.41, 37.59, 76.41, 5.31, -34.23, 1.11, 1.10, 23.46, 635.47, -876.32, 467.83, 62.25 };

        // Sort numbers ascending by numerical value
      sort(num, (one, two) => one < two);
      //prints numbers in ascending order
        Console.WriteLine("Sorted numbers (ascending):");
        Console.WriteLine(string.Join(",", num));

      // Sort people alphabetically (lexicographically) by name
      sort(names, (one, two) => string.Compare(one.Name, two.Name) < 0);
      //prints out names in alphabetical order
      Console.WriteLine("Sorted people (alphabetically by name):");
      foreach (var person in names){
          Console.Write($"{person.Name}, {person.Age} ");
          Console.WriteLine();
      }
      Console.WriteLine();
      // sort people descending by age, where people of the same age should be sorted alphabetically (lexicographically)
      sort(names, (a, b) => a.Age > b.Age || (a.Age == b.Age && string.Compare(a.Name, b.Name) > 0));
      //prints ages in numerical order and in alphabetically in the case of people with the same age
      Console.WriteLine("Sorted people (descending by age, then alphabetically by name):");
      foreach (var person in names){
          Console.Write($"{person.Name}, {person.Age} ");
          Console.WriteLine();
      }
  }
}