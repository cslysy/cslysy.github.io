---
layout: post
title:  "Do not complicate - One-time variables"
date:   2016-06-12 12:00
categories: Basics
disqus: true
---
We as a programmers tend to complicate things. A lot and very often. There are so many aspects of our job when we gets blinded and do the unnecessary things here and there. This activities introduces complication, distraction and absurdity.
For this reason, I decided to start with "Do not complicate" series. First of all, Im aware that some topics, if not the most, are very basic so please do not call me [Captain Obvious][captain-obvious] :) just read on and find out that despite it's simplicity  so many programmers making that fundamental mistakes again and again, including myself.

In this post I would like to cover one-time variables topic. By one-time variable I mean variable that is used only one time after its declaration.

Here it is:

```java
public void buildPanel(){
  Label nameLabel = new Label("name");
  panel.add(nameLabel);
}
```


Each of us seen such a code, it is very common, no matter how experienced authors are. There are cases when using one-time variable is reasonable but I touch this topic later. For now, let's take a look at this famous quote:


>Perfection is Achieved Not When There Is Nothing More to Add, But When There Is Nothing Left to Take Away”

<div align="right" style="margin-top:-20px; font-size:0.8em; margin-right:50px">
Antoine de Saint-Exupéry
</div><br>

Programming-specific translation may be formed as:

>Code is perfect not when there is nohing to be add, but when there is nothing left to take away without breaking it and losing readability


So code from the example should be written as:

```java
public void buildPanel(){
  panel.add(new Label("name"));
}
```


## One-time variables and it's influence on code design

Avoiding one-time variable may influence our code design in a very positive way because it favour immutability and make the code more composable.

Imagine that Label has only no-arguments constructor:

```java
public void buildPanel(){
  Label nameLabel = new Label();
  nameLabel.setText("name");
  panel.add(nameLabel);
}
```

Variable `nameLabel` is used twice but in fact it is hidden one-time variable as invoking setter is a part of object initialization. We should always favour constructor initialization over setters. When we follow these rules in case of more complicated object structure then advantages are much more visible.

Let's take a look on these three simple classes implemented in mutable way and used one-time variables:

```java
interface SshAuthMethod {
  void authenticate();
}

class UsernamePasswordAuthMethod implements SshAuthMethod {

  private String userName;
  private String password;

  @Override
  public void authenticate() {}

  //getters and setters
}

class SshConnection {

  private String hostname;
  private int port;
  private SshAuthMethod authMethod;

  public void connect(){}

  //getters and setters
}

class Application {

  public static void main(String[] args) {
      UsernamePasswordAuthMethod authMethod = new UsernamePasswordAuthMethod();
      usernamePasswordAuthMethod.setUserName("OptimusPrime");
      usernamePasswordAuthMethod.setPassword("LoveMegatron");
      SshConnection sshConnection = new SshConnection();
      sshConnection.setHostname("http://planete.erath.com");
      sshConnection.setPort(123);
      sshConnection.setAuthMethod(usernamePasswordAuthMethod);
      sshConnection.connect();
  }
}
```

And immutable code without one time variables:

```java

class UsernamePasswordAuthMethod implements SshAuthMethod {

  private final String userName;
  private final String password;

  public UsernamePasswordAuthMethod(String userName, String password) {
      this.userName = userName;
      this.password = password;
  }

  @Override
  public void authenticate(){ }

  //getters
}

class SshConnection {
  private final String hostname;
  private final int port;
  private final SshAuthMethod authMethod;

  public void connect(){}

  public SshConnection(String hostname, int port, SshAuthMethod authMethod) {
      this.hostname = hostname;
      this.port = port;
      this.authMethod = authMethod;
  }

  //getters
}

class Application {

  public static void main(String[] args) {
      new SshConnection(
        "http://planete.erath.com", 123,
        new UsernamePasswordAuthMethod("OptimusPrime", "LoveMegatron")
      ).connect();
  }
}
```

As you can see, making classes immutable and avoiding one-time variables leads us to the very simple and straightforward code. There may be some drawbacks like too many parameters for constructor. In this case you should rethink your class design. If it is possible to break the code into the smaller classes just do it, if not then introduce Builder pattern. The second issue may be code code formatting. In this case I suggest to read on my [thoughts on code formatting][code-formatting], especially excerpt about paired brackets notation.

## When to use one-time variables


bla bla bla


## Summary
That's all when it comes to my approach to code formatting. I hope that all arguments and advices convinced you to be more careful on that topic. Even nowadays, we are very close to punched card approach and their 80 characters in line :)

If you know other ways to improve code formatting please share you experience in comments. Like this post? Stay tuned by [subscribing][feed] my further blog activities!

[captain-obvious]:https://en.wikipedia.org/wiki/Captain_Obvious
[code-formatting]: http://cslysy.github.io/basics/2016/04/12/code_formatting.html
[feed]:{{ site.baseurl }}feed.xml
