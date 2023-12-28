def custom_sort(list):
    return list[1]

numbers = [645.41, 37.59, 76.41, 5.31, -34.23, 1.11, 1.10, 23.46, 635.47, -876.32, 467.83, 62.25]

people = [("Hal", 20), ("Susann", 31), ("Dwight", 19), ("Kassandra", 21), ("Lawrence", 25), ("Cindy", 22),
            ("Cory", 27), ("Mac", 19), ("Romana", 27), ("Doretha", 32), ("Danna", 20), ("Zara", 23),
            ("Rosalyn", 26), ("Risa", 24), ("Benny", 28), ("Juan", 33), ("Natalie", 25)]

numbers.sort()
print("Numbers of list in ascending order: ", numbers)
print()
people.sort()
print("People of list in ascending order: ", people)
print()
people.sort(key=custom_sort, reverse=True)
print("Age in descending order: ", people)