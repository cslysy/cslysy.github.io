---
layout: post
title:  "Do not complicate - One-time variables"
date:   2016-05-15 12:00
categories: Basics
disqus: true
---
We as a programmers tend to complicate things. A lot and very often. There are so many aspects of our job when we gets blinded and do the unnecessary things here and there. This activities introduces complication, distraction and absurdity.
For this reason, I decided to start with "Do not complicate" series. First of all, I'm aware that some topics, if not the most, are very basic so please do not call me [Captain Obvious][captain-obvious], just read on and find out that despite its simplicity so many programmers makes that fundamental mistakes again and again, including myself.

In this post I would like to cover one-time variables topic. By one-time variable I mean variable that is used only once after its declaration.

Here it is:

```java
public void buildPanel(){
  Label nameLabel = new Label("name");
  panel.add(nameLabel);
}
```

Each of us have seen such a dirty code. It is very common, no matter how experienced authors are. There are cases when using one-time variable is reasonable but I touch this topic later. For now, let's take a look at this famous quote:


>Perfection is Achieved Not When There Is Nothing More to Add, But When There Is Nothing Left to Take Away

<div align="right" style="margin-top:-20px; font-size:0.8em; margin-right:50px">
Antoine de Saint-Exupéry
</div><br>

Programming-specific translation may be formed as:

>Code is perfect not when there is nohing to be add, but when there is nothing left to take away without breaking it and losing readability


Following this rule, code from the example should be written as:

```java
public void buildPanel(){
  panel.add(new Label("name"));
}
```

We could take away some code without losing readability. Keep in mind this sentence whenever you are going to define new variable and ask yourself - do I really need it?


## One-time variables and its influence on code design

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

Let's take a look on these three simple classes implemented in mutable way:

```java
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

  private String userName;
  private String password;

  public UsernamePasswordAuthMethod(String userName, String password) {
      this.userName = userName;
      this.password = password;
  }

  @Override
  public void authenticate(){ }

  //getters
}

class SshConnection {
  private String hostname;
  private int port;
  private SshAuthMethod authMethod;

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

As you can see, making classes immutable and avoiding one-time variables leads us to the very simple and straightforward code. You may encounter some obstacles along the way like constructor with too many parameters, in this case you should rethink your class design. If it is possible to break the code into the smaller classes just do it, if not then introduce [Builder][builder] pattern. The second issue may be code formatting of highly composable, variable-free code where I suggest you to read on my [thoughts on code formatting][code-formatting], especially excerpt about paired brackets notation.

## When to use one-time variables

As I mentioned before, there are places for one-time variable because sometimes we are not able to express our intentions without it.

Does this tell you something?

```java
logger.info(
  "My favourite day of 2016 is {}",
  LocalDate.of(2016, Month.SEPTEMBER, 12)
);
```

and now:

```java
LocalDate dayOfProgrammer = LocalDate.of(2016, Month.SEPTEMBER, 12);
logger.info("My favourite day of 2016 is {}", dayOfProgrammer);
```

This can be seen even more clearly in unit tests:

```java
String expectedMessage = "Too many results";
int anyInt = 9876633;
Person lastPerson = persons.get(list.size() - 1);
```

In short, if you think that code responsible for creating variable is not sufficient to express your intention then do not hesitate to introduce one-time variable.

## Summary
That's all if it comes to my thoughts on this topic. As I mentioned before, we tend to complicate our code a lot and very often so if you found this post interesting stay tuned by [subscribing][feed] my further blog activities.

[captain-obvious]:https://en.wikipedia.org/wiki/Captain_Obvious
[code-formatting]: http://cslysy.github.io/basics/2016/04/12/code_formatting.html
[builder]:https://en.wikipedia.org/wiki/Builder_pattern
[feed]:{{ site.baseurl }}feed.xml
