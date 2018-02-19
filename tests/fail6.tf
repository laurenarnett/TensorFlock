// Wrong type in the index
anInt : Int;
anInt = myTensor[1.2, 3.4, 5.6]

// Nonsensical indices
anotherInt : Int;
anotherInt = myTensor[[1, 2] [3]]

// Empty index
anInt : Int;
anInt = myTensor[]

// Global Scope usage
myTensor[1, 2, 3]

