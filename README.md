# word-trie

API:
- (make-trie): creates a new empty trie.
- (add-word string trie): adds a word, in lower case, to a trie node and returns the trie. The trie should be destructively modified.
- (subtrie trie char1 char2 ...): given a trie and zero or more characters, returns the root of the subtrie for that sequence of characters, if any, or nil.
- (trie-word trie): returns the word at a trie node if any, or nil.
- (trie-count trie): returns the number of words stored at or under this trie node.
- (mapc-trie fn trie): given a function and a trie, calls (fn char subtrie) with the character and subtrie for every branch immediately under trie. Called for effect. Returns trie.
- (read-words file trie): Reads a file of words into trie. Returns trie. The file should contain one word per line.

To run the test code, you'll need to put the Moby Project's crosswd.txt in the same directory as it. Also, compile the trie code before trying to load the entire dictionary.

Read more about tries [here](https://en.wikipedia.org/wiki/Trie). Project based on description [here](http://www.cs.northwestern.edu/academics/courses/325/exercises/challenges.php#word-trie).
