---
layout: post
title:  "Code formatting or There and Back Again"
date:   2016-03-07 13:00
categories: Basics
disqus: true
---
Today I would like to take you on a journey into the times where [Alexander Fleming][fleming] accidentally rediscovers the antibiotic [Penicillin][penicillin], British inventor [John Logie Baird][johnl] demonstrates the world's first colour television transmission and Software was developed using [punched card][punched_card].

Thats right, programmers were marking numbers in specified rows and columns in extremely powerful IDE called IBM PORT-A-PUNCH:

<a title="By Journey234 (Own work) [Public domain], via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File%3AIBM_Port-A-Punch.jpg"><img width="512" alt="IBM Port-A-Punch" src="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e5/IBM_Port-A-Punch.jpg/512px-IBM_Port-A-Punch.jpg"/></a>

No debugging, no unit test, no mercy! At least, they didn't have to worry about code formatting, only pure logic and card that encodes data, most commonly 80 characters.

## Displays revolution

Things start changing when computers got displays that could be used in so-called "Text mode" where content was represented as characters instead of individual pixels. As it turned out one of the most common text mode support 80x25 characters grid. See punched card inspiration? Definitely!

<a title="By Ruben de Rijcke (Own work) [CC BY-SA 3.0 (http://creativecommons.org/licenses/by-sa/3.0)], via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File%3AIbm_pc_5150.jpg"><img width="512" alt="Ibm pc 5150" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f1/Ibm_pc_5150.jpg/512px-Ibm_pc_5150.jpg"/></a>



Together with displays more advanced hight level programming languages like [COBOL][cobol] and [Fortran][fortran] was invented. Finally programmers were able to write the code using english-like syntax:


````cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
ENVIRONMENT DIVISION.
DATA DIVISION.
PROCEDURE DIVISION.
PARA-1.
DISPLAY "Hello world!".
EXIT PROGRAM.
END PROGRAM HELLO-WORLD.
````

This syntax is, to say the least, ugly. Let's see what other said about COBOL:

>The use of COBOL cripples the mind; its teaching should therefore be regarded as a criminal offense.

<div align="right" style="margin-top:-20px; font-size:0.8em; margin-right:50px">
E.W. Dijkstra
</div>

>I knew I'd hate COBOL the moment I saw they'd used 'perform' instead of 'do'.

<div align="right" style="margin-top:-20px; font-size:0.8em; margin-right:50px">
Larry Wall creator of the Perl
</div>

## Nowadays

Thankfully, COBOL did not become industry standard. These days programmers have way better programming languages with tons of libraries and really powerful IDE's. Moreover, there is no limitation of 80 characters in line thanks to amazing widescreen displays. Today our code may look like that:

![alt tag](/assets/images/code-formatting/widescreen.png)

That's the part of [AbstractNestablePropertyAccessor][spring-abstract] class from widely known [spring-framework][spring] library.
For more inspiration please see [spring github repo][spring-gh]. Many developers would say that there is nothing wrong with this code and that's the point of this article. Don't get me wrong, I really like Spring, but I'm wondering if someone executed some kind of [uglyfier][uglyfier] on that code :) This is very common practice nowadays to write the code like that. I asked developers may times why the write code this way? Why so many characters in one line? The most common answer is "Because we can, we have widescreens!" then Im asking if they tried to open it on mobile device?

<img src="/assets/images/code-formatting/iphone.png" style="width:100%; display: block; margin: 0 auto;"/>

or diff in side-by-side view during code review or merging?

<img src="/assets/images/code-formatting/side-by-side-compare.png" style="width:100%; display: block; margin: 0 auto;"/>

or:

- debugging when many debug panels are open
- attach in presentation/blog/book
- print it

Moreover, it is known truth that code is read much more often than it is written. For this reason we should do the best to keep our code in a good shape.

It these arguments are not enough Im getting a white flag and give up.

<img src="/assets/images/code-formatting/code-formatting-art.jpg" style="width:100%; display: block; margin: 0 auto;"/>

But if you are starting to change you mind and you are interested on how to deal with code formatting issue please keep on reading.


## Improve code formatting

First of all we need to configure IDE properly.

#### Use spaces instead of tabs for code indentation

This is very common issue. Tab size is not a constant between IDE therefore code may look differently depending on code editor.
This is for example the same code open in three different editors:

Eclipse:
<img src="/assets/images/code-formatting/eclipse.png" style="width:100%; display: block; margin: 0 auto;"/>
Netbeans:
<img src="/assets/images/code-formatting/netbeans.png" style="width:100%; display: block; margin: 0 auto;"/>
GitHub:
<img src="/assets/images/code-formatting/github.png" style="width:100%; display: block; margin: 0 auto;"/>

I bet that this file has been saved using Eclipse :)

#### Set proper indentation size

4 spaces is most common but I would rather suggest to use 3 instead. Let's take a look on following examples:

4 spaces indentation
<img src="/assets/images/code-formatting/4spaces.png" style="width:100%; display: block; margin: 0 auto;"/>
3 spaces indentation
<img src="/assets/images/code-formatting/3spaces.png" style="width:100%; display: block; margin: 0 auto;"/>

See the difference? We are saving some space without losing readability.

#### Use right margin

This odd line that goes down the middle of your code is very useful feature.

<img src="/assets/images/code-formatting/right-margin.png" style="width:100%; display: block; margin: 0 auto;"/>

If you code starting to reach the right margin barrier it is the first sign that we are going wrong with formatting. You should stop then for a moment and try to identify if you can break the line in reasonable spot . If not that's fine and don't worry about it to much, just be prepared to elaborate you decision in code review stage.

My personal preference is set right margin to 80 characters and max characters in line to 100. In addition, you should never rely on automatic code formatter when it comes to line break, it is are usually very bad on that.

Automatic code formatter line breaks:

<img src="/assets/images/code-formatting/ide-formatting.png" style="width:100%; display: block; margin: 0 auto;"/>

Manual line breaks:

<img src="/assets/images/code-formatting/manual-formatting.png" style="width:100%; display: block; margin: 0 auto;"/>

Usually, when code could not break without losing readability there is something wrong with design.








[fleming]: https://en.wikipedia.org/wiki/Alexander_Fleming
[penicillin]: https://en.wikipedia.org/wiki/Penicillin
[johnl]: https://en.wikipedia.org/wiki/John_Logie_Baird
[punched_card]:https://en.wikipedia.org/wiki/Punched_card
[cobol]:https://en.wikipedia.org/wiki/COBOL
[fortran]:https://en.wikipedia.org/wiki/Fortran
[uglyfier]:https://github.com/mishoo/UglifyJS
[wat]:http://memesvault.com/wat-meme-old-lady/
[spring-abstract]:https://github.com/spring-projects/spring-framework/blob/master/spring-beans/src/main/java/org/springframework/beans/AbstractNestablePropertyAccessor.java
[spring]:https://projects.spring.io/spring-framework/
[spring-gh]:https://github.com/spring-projects
[feed]:{{ site.baseurl }}feed.xml
