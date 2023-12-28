/*
  Writing a sorting algorithm that can handle any data types.
  Then test the algorithm with test cases
    - sort numbers ascending by numerical value,
    - sort people alphabetically (lexicographically) by name, and to
    - sort people descending by age, where people of the same age should be sorted alphabetically (lexicographically).
*/

// Include necessary libraries
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Initializes a structure with an array for the student's name and an integer for their age
typedef struct {
    char name[50];
    int age;
} Person;

int compare_float(const void *a, const void *b) {
    return (*(float *)a - *(float *)b);
}

int compare_people(const void *a, const void *b) {
    //Compares Strings if they are equal
    return strcmp(((Person *)a)->name, ((Person *)b)->name);
}

int compare_people_descending_age(const void *a, const void *b) {
    if (((Person *)a)->age != ((Person *)b)->age) {
        return ((Person *)b)->age - ((Person *)a)->age;
    }
    //Compares strings alphabetically if ages are equal
    return strcmp(((Person *)a)->name, ((Person *)b)->name);
}

int main() {
    float numbers[] = {645.41, 37.59, 76.41, 5.31, -34.23, 1.11, 1.10, 23.46, 635.47, -876.32, 467.83, 62.25};
    Person people[] = {{"Hal", 20}, {"Susann", 31}, {"Dwight", 19}, {"Kassandra", 21}, {"Lawrence", 25},
                       {"Cindy", 22}, {"Cory", 27}, {"Mac", 19}, {"Romana", 27}, {"Doretha", 32},
                       {"Danna", 20}, {"Zara", 23}, {"Rosalyn", 26}, {"Risa", 24}, {"Benny", 28}, {"Juan", 33},
                       {"Natalie", 25}};

    int num_numbers = sizeof(numbers) / sizeof(float);
    int num_people = sizeof(people) / sizeof(Person);

    // Sort numbers ascending by numerical value
    qsort(numbers, num_numbers, sizeof(float), compare_float);

    printf("Sorted numbers (ascending):\n");
    for (int i = 0; i < num_numbers; i++) {
        printf("%.2f ", numbers[i]);
    }
    printf("\n\n");

    // Sort people alphabetically by name
    qsort(people, num_people, sizeof(Person), compare_people);

    printf("Sorted people (alphabetically by name):\n");
    for (int i = 0; i < num_people; i++) {
        printf("%s, %d\n", people[i].name, people[i].age);
    }
    printf("\n");

    // Sort people descending by age, then alphabetically by name
    qsort(people, num_people, sizeof(Person), compare_people_descending_age);

    printf("Sorted people (descending by age, then alphabetically by name):\n");
    for (int i = 0; i < num_people; i++) {
        printf("%s, %d\n", people[i].name, people[i].age);
    }

    return 0;
}