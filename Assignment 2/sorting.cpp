/*
  Writing a sorting algorithm that can handle any data type 
  Then test the algorithm with test cases
    - sort numbers ascending by numerical value,
    - sort people alphabetically (lexicographically) by name, and to
    - sort people descending by age, where people of the same age should be sorted alphabetically (lexicographically).
*/

// Including libraries to output and using strings
#include <iostream>
#include <string>

// Creating "Person" data type
struct Person {
    std::string name;
    int age;
};

// Merge function for merging two sorted halves of an array
template <typename T, typename Compare>
void merge(T array[], int left, int mid, int right, Compare comp) {
    int subArr1 = mid - left + 1;
    int subArr2 = right - mid;

    //temporary arrays get created
    T* leftArr = new T[subArr1];
    T* rightArr = new T[subArr2];

    for (int i = 0; i < subArr1; i++) {
          leftArr[i] = array[left + i];
    }
    for (int j = 0; j < subArr2; j++) {
          rightArr[j] = array[mid + 1 + j];
    }
    int i = 0; 
    int p = 0; 
    int t = left;
    
    //Merges the temporary arrays back into the main array
    while (i < subArr1 && p < subArr2) {
        if (comp(leftArr[i], rightArr[p])) {
              array[t] = leftArr[i];
            i++;
        } else {
              array[t] = rightArr[p];
              p++;
        }
          t++;
    }

    while (i < subArr1) {
          array[t] = leftArr[i];
        i++;
          t++;
    }

    while (p < subArr2) {
          array[t] = rightArr[p];
          p++;
          t++;
    }

    delete[] leftArr;
    delete[] rightArr;
}

// Left index indicates the start and right the end
template <typename T, typename Compare>
void mergeSort(T arr[], int left, int right, Compare comp) {
    if (left < right) {
        int mid = left + (right - left) / 2;
        mergeSort(arr, left, mid, comp);
        mergeSort(arr, mid + 1, right, comp);
        merge(arr, left, mid, right, comp);
    }
}

//Main function serves to call the functions and subsequently test them
int main() {
  //samples for test
    float num[] = {645.41, 37.59, 76.41, 5.31, -34.23, 1.11, 1.10, 23.46, 635.47, -876.32, 467.83, 62.25};
    Person people[] = {
        {"Hal", 20}, {"Susann", 31}, {"Dwight", 19}, {"Kassandra", 21}, {"Lawrence", 25},
        {"Cindy", 22}, {"Cory", 27}, {"Mac", 19}, {"Romana", 27}, {"Doretha", 32}, {"Danna", 20},
        {"Zara", 23}, {"Rosalyn", 26}, {"Risa", 24}, {"Benny", 28}, {"Juan", 33}, {"Natalie", 25}
    };

    // 
    int num_count = sizeof(num) / sizeof(num[0]);
    int people_count = sizeof(people) / sizeof(people[0]);

    // Sort numbers ascending by numerical value
    mergeSort(num, 0, num_count - 1, [](const float& one, const float& two) {
        return one < two;
    });
    //prints numbers in ascending order
    printf("Sorted numbers (ascending):\n");
    for (int i = 0; i < num_count; i++) {
        std::cout << num[i] << " ";
    }
    printf("\n\n");

    // Sort people alphabetically (lexicographically) by name
    mergeSort(people, 0, people_count - 1, [](const Person& one, const Person& two) {
        return one.name < two.name;
    });
    //prints out names in alphabetical order
    printf("Sorted people (alphabetically by name):\n");
    for (int i = 0; i < people_count; i++) {
        std::cout << people[i].name << ", " << people[i].age;
        printf("\n");
    }
    printf("\n\n");

    // sort people descending by age, where people of the same age should be sorted alphabetically (lexicographically)
    mergeSort(people, 0, people_count - 1, [](const Person& one, const Person& two) {
        if (one.age == two.age) {
            return one.name > two.name;
        }
        return one.age > two.age;
    });

    //prints ages in numerical order and in alphabetically in the case of people with the same age
    printf("Sorted people (descending by age, then alphabetically by name):\n");

    for (int i = 0; i < people_count; i++) {
        std::cout << people[i].name << ", " << people[i].age;
        printf("\n");
    }
    printf("\n\n");

    return 0;
}