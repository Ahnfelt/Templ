Templ readme
============

![Conceptual overview](http://imgur.com/Glaru.png)

Templ is a language for specifying document templates.
The most common usage is to generate HTML documents on the server side.
The idea is that you specify a template and then apply the template to
different values. The following is a small template:

    <a href="mailto:$person.email">$person.name</a> | $person.name

In the above, we're accessing the `.email` and `.name` fields of the 
`$person` variable. The vertical bar `|` means that the code to the 
left and the right are *alternatives*. If the left alternative tries
to access a field that isn't in the record, the right alternative is
run instead.

Applying this template to the record `(name: {Brenda}, email: {brenda$@example.com})`
yields `<a href="mailto:brenda@example.com">Brenda</a>`, whereas applying it to
the record `(name: {Peter})` just yields `Peter`; 
the right alternative is used because the `.email` field is missing.

If you're familiar with other template engines, you might notice that Templ
is rather more concise. In JSP the above would be written

    <% if(person.getEmail() != null) { %>
        <a href="mailto:<%= person.getEmail() %>"><%= person.getName() %></a>
    <% } else { %>
        <%= person.getName() %>
    <% } %>

The JSP code is more verbose primarily because it needs an explicit 
check for wether or not the email exists, requirering you to repeat
the field access or introduce a temporary variable.


Ruling out failure
------------------

Both the Templ and the JSP examples beg the question; what happens if the 
`.name` field is missing? Some template engines will report an error to
the end user, some will insert a bogus string such as "null", and some will 
insert the empty string. Either way, the end user is likely to see a page 
that is broken. The problem is that traditionally there is no way to 
distinguish between a field that is always present and a field that might 
sometimes be missing.

Templ has a type system that supports this distinction. Fields of records
are either required or optional. If a field is optional, it can only be 
accessed inside a left alternative. In other words, when a field can be 
missing, there must always be a backup plan. In the example, we can 
conclude that `.name` is a required field, while `.email` is likely 
optional. Such a type looks like this: 

    (a.name: String, a.email?: String => a)
    
It must be some type `a` that has a `.name` field of type `String` and 
optionally an `.email` field also of type `String`.

Templ has full type inference, so you *never* need to write down a type.
Instead, it should be seen as an error detection mechanism that rules out 
failures - if the template compiles, it cannot be the source of an exception. Ever.

Other than records, the type system supports lambda functions, lists
and strings.


Variables and functions
-----------------------

*To be written, but here's a bit:*

You can bind a variable to a value for the remainder of the 
current scope like this:

    @local $hello {Hello, World!}
    <h1>$hello</h1>
    <p>$hello</p>

Or in a delimited scope like this:

    @let $hello {Hello, World!} {<strong>$hello</strong>}

Functions are defined like this:

    @function Header $title {<h1>$title</h1>}
    
And called like this:
    
    @Header {Hello, World!}

You can also write anonymous functions like this:

    @lambda $title {<h1>$title</h1>}

And apply them like this:

    @apply $f {Hello, World!}


Iterating over lists
--------------------

*To be written, but here is an example:*

    <ul>
    @for $item $myList {
        <li>$item</li>
    }
    </ul>


Syntactic details
-----------------

*To be written, but here's a bit:*

You escape the special characters `$@{}|` by prefixing them
with a `$`, eg. `peter$@example.com` or `$$23.00`.

Dots `.` are only special when simultaneously appearing in extension of a
variable or lookup and followed by a valid identifier (no whitespace).
For example,

    Hello, $person.name.first.

Will contian the final dot `.` (assume $person.name.first == {Jack}):

    Hello, Jack.

If you need to end an expression early, curly braces work like 
parenthesis in text mode:

    Hello, {$person.name}.first.

Will display as (assume $person.name == {Peter}):

    Hello, Peter.first.

Whitespace is ignored inside commands, but not in text mode.

Strings are made by using textmode curly braces, eg. `{Copenhagen}`.

List literals are enclosed in brackets `[]` and comma-delimited, eg. `[{a}, {b}, {c}]`.

Calling templates from Java
---------------------------

*To be written*

