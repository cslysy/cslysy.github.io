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

No debugging, no unit test, no mercy! At least, you didn't have to worry about code formatting. Only you and card that encodes data, most commonly 80 characters.

Things gets changed when computers became able to display data on the screen. Awesome!

<a title="By Ruben de Rijcke (Own work) [CC BY-SA 3.0 (http://creativecommons.org/licenses/by-sa/3.0)], via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File%3AIbm_pc_5150.jpg"><img width="512" alt="Ibm pc 5150" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f1/Ibm_pc_5150.jpg/512px-Ibm_pc_5150.jpg"/></a>


###Background
[NPM][npm] (Node package manager) has been invited to help [Node.js][node.js] developers manage their javascript dependencies. It turns out that npm can be successfully used in projects not related to the Node.js technology. Let's leave Node topic for one of the further posts. For know, you should know that __Node.js is a javascript runtime__ (using [V8][v8] engine internally), providing so-called event-driven, non-blocking I/O model designed to build scalable network applications.
<div class="tip">
If you are still interested what Node.js is in details I highly recommend you to watch [this][what-is-node] video.
</div>
###NPM installation
NPM can be installed together with Node.js using installer from the official [website][node.js] (npm is written entirely in javascript and use Node.js as a runtime environment). Mac or Linux users may use integrated package manager, npm is available in [homebrew][homebrew] or [yum][yum]. To check if npm is installed you can simply run:
{% highlight bash %}
npm -v
{% endhighlight %}
###Playing with NPM
Let's assume that our project require jQuery. To download it just execute
{% highlight bash %}
npm install jquery
{% endhighlight %}
NPM should create *node_modules/jquery* directory containing jquery files. Piece of cake.
But our project is growing! It turns out that we need also bootstrap and underscore. We can install them using the same command as before, but this is not convenient when we need to share our project with others. It would be great if we can just define all the dependencies within single configuration file (like [pom.xml][pom] from maven). It is possible by project.json descriptor. We can create it manually or using interactively command. To do so, just type
{% highlight bash %}
npm init
{% endhighlight %}
and follow the prompts.

Example file may look as follows

{% highlight json %}
{
  "name": "awesome-modern-web-project",
  "version": "1.0.0",
  "description": "some project description",
  "main": "index.js",
  "dependencies": {
    "jquery": "^2.2.0"
  },
  "devDependencies": {},
  "scripts": {
    "test": "make test"
  },
  "author": "cslysy",
  "license": "ISC"
}
{% endhighlight %}

For now, the most important part is *dependencies* and *devDependencies*. Within *dependencies* we can define our runtime project dependencies whereas *devDependencies* defines libraries required to develop like coffeescript to javascript transpiler or unit-test support.

New dependencies can be added in a two ways, manually or using a command. In order tell npm that we need bootstrap just add it within *dependencies*:
{% highlight json %}
{
  "dependencies": {
    "jquery": "^2.2.0",
    "bootstrap": "^3.3.6"
  }
}
{% endhighlight %}
and execute
{% highlight bash %}
npm install
{% endhighlight %}
As a result, npm will download bootstrap. As it was mentioned before we can do the same using npm command
{% highlight bash %}
npm install underscore --save
{% endhighlight %}
The *--save* argument tells npm to add underscore definition to *dependencies* section after downloading.
If you need some libraries only for development support just define them in *deveDependencies* or execute
{% highlight bash %}
npm install coffee-script --saveDev
{% endhighlight %}
That's the way to install developer dependencies. What if we decide to use coffe-script in other projects, do we need to install it again? The answer is NO. Coffee-script transpiler is only a tool. Do you install Netbeans or other favourite IDE for every single java project you are working on? NPM let's us decide whether we want to use package locally (single project scope) or globally. By default packages are installed locally. In order to install package globally use *-g* (or *--global*) mode
{% highlight bash %}
npm install some-pakage-name -g
{% endhighlight %}
This command may require some write permission. If you receive EACCES error try to fix npm permissions according to [this][fix-permission] how to.

###Conclusions
Node package manager is a great tool that help us control all the mess related to project dependencies management. Someone may say that NPM is not the best choice for the web projects favoring [Bower][bower] as a better one. If you are interested what is wrong with Bower please read [Why We Should Stop Using Bower][wrong-with-bower].

###What's next
Our dependencies are downloaded but how to use them? Linking them in our project by pointing into *node_modules/modulename* sounds at least strange. Dependencies should be copied somewhere creating, together with other files, target project structure ready to be deployed to development or production environment. Copying all of them manually is pure madness. That's the topic for the next experiment. How to automate copy or minification processes? How to build project distribution? Stay in touch by [subscribing][feed] my blog activities!





[fleming]: https://en.wikipedia.org/wiki/Alexander_Fleming
[penicillin]: https://en.wikipedia.org/wiki/Penicillin
[johnl]: https://en.wikipedia.org/wiki/John_Logie_Baird
[punched_card]:https://en.wikipedia.org/wiki/Punched_card
[feed]:{{ site.baseurl }}feed.xml
