#Lab 1
Answer to Q1: 
: (a) pi at line 4 is bound by the one in line 3. While the one in line 7 is bound by line 1. Because if there is a definition inside the function it well take that before taking the definition outside the function.
: (b) x at line 3 and 10 is bound by the input at line 2. x at 6 is bound at line 5 by the case. x at 13 is bound by the at line 1.

**Answer to Q2:**

    return is ((Int, Int), Int) because it is either
    (b, 1):((Int, Int), Int)
    	b:(x, 3)
	    	(x, 3):(Int, Int)
	    	x:Int
		    3:Int
		    
    	1:Int
    or
    (b, a+2):((Int, Int), Int)
    	b:(x, 3)
	    	(x, 3):(Int, Int)
	    	x:Int
		    3:Int
		    
    	a + 2:Int
    		a:Int
    		2:Int

Include your writeup for the Lab 1 questions here. Use correct
Markdown markup. For more details, start with the article
https://help.github.com/articles/github-flavored-markdown
