# word-trie

API:
    (make-trie): creates a new empty trie.
    (add-word string trie): adds a word, in lower case, to a trie node and returns the trie. The trie should be destructively modified.
    (subtrie trie char1 char2 ...): given a trie and zero or more characters, returns the root of the subtrie for that sequence of characters, if any, or nil.
    (trie-word trie): returns the word at a trie node if any, or nil.
    (trie-count trie): returns the number of words stored at or under this trie node.
    (mapc-trie fn trie): given a function and a trie, calls (fn char subtrie) with the character and subtrie for every branch immediately under trie. Called for effect. Returns trie.
    (read-words file trie): Reads a file of words into trie. Returns trie. The file should contain one word per line.

The goal of this task is to make a very fast word lookup tool. The next two tasks apply this tool.

The trick is to pre-process the dictionary to make word lookup very fast, even faster than binary search on a sorted array, but, unlike a hashtable, capable of looking up words letter by letter, so that we can very quickly reject a sequence of letters that can't be a word, like "qp...".

A good data structure for this is a trie. A trie is a recursive tree data structure. The root node of the trie has a branch for every letter that starts at least one word in English. Each branch leads to a subtrie with branches for the second letters that can follow the first letter. Thus, under the subtrie for "q" will be branches for just those letters that can follow "q" in English.

A trie is easily built recursively. You start with an empty trie and insert words one by one from a word list. Inserting a word means going down the trie, letter by letter, following a branch if one already exists, or adding one if not. You add the word to the node reached by the last letter. A node can have at most one word, but a node can have both a word and subtries, e.g., "aft" reaches a node that has the word "aft" but also subtries leading to "after" and others.

Use a CLOS class or structure for trie nodes, with a print function that just shows the word in the trie node, if any, and how many words are at or under the trie. You don't want a trie to print its entire contents every time you look at it!

To run the test code, you'll need to download the Moby Project's crosswd.txt and put it in the same directory as your test code.

Compile your trie code before trying to load the entire dictionary!
