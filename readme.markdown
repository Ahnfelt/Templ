Templ readme
============

![Conceptual overview](http://imgur.com/Glaru.png)


Introduction
------------

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


Modules and imports
-------------------
*To be written*


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


Escaping and XSS prevention
---------------------------

*To be implemented, but here's the idea*

User input may contain characters that have special meaning in the document that's being
generated. For example, the symbols `<>/&"'` are special in HTML. If we insert text 
containing these, the best case scenario is that the page displays wrong. The worst case
scenario is that the user supplied a malicious input, such as a `<script>...</script>`,
which could enable him to execute code under the credentials of another user. This 
security exploit is known as [XSS](http://en.wikipedia.org/wiki/Cross-site_scripting),
and is one of many related code execution vulnerabilities.

Although some sites filter these characters from the input, we cannot recommend this 
route. First, it swallows completely valid user input; this is why code examples
are sometimes swallowed on forums and blogs. Second, you cannot know which characters are
dangerous unless you know exactly how the data is going to be used in your entire code
base now and in all future; what to filter for HTML is different from what to filter in
SQL, Bash, CSS, JSON, YAML, or whatever else you might need some day. 

Instead, escape user input when *using* it, because only at that point can you know what 
characters are problematic and how to properly escape them without loosing any data. 
It is unfortunately quite error prone (and as such a security risk) to manually escape 
every usage of user input, but often libraries will relieve you of this tedious duty -
for example, SQL libraries typically have parameterized or prepared statements that 
automatically escape all parameters.

In Templ, all strings that are inserted are automatically escaped using the innermost 
escape mechanism that is in scope. The default escape mechansim is `@html`, but let's 
make it explicit: 

    @html {
        <h1>$title</h1>
    }
    
If $title is `Why 0 < 1`, the result will be `<h1>Why 0 &lt; 1</h1>`. The less than `<`
character was automatically escaped using the @html escape mechanism. Sometimes it is
desirable to insert code fragments, and in that case you can use @raw. For example, 

    @raw $title
    
would emit `Why 0 < 1`.


Translation and localization
----------------------------

*To be implemented, but here's the idea*

Templ supports sentence-based translation. To mark a sentence for translation, prefix
it with `@` and enclose it in curly braces, eg.

    <h1>@{Welcome to my site!}</h1>

By default, the text inside the braces will be shown. However, a separate translation
file like this can be used (similar to [gettext](http://www.gnu.org/software/gettext/manual/gettext.html#PO-Files)):

    english {Welcome to my site!}
    danish {Velkommen til min side!}

If `danish` is chosen as the language, the template yields `<h1>Velkommen til min side!</h1>`.

Translation braces can be parameterized. Anything but plain text and escape sequences are 
considered parameter values. For example,

    @{I've got $data.count apples!}

Would fit the translation strings:

    english {I've got $number apples!}
    danish {Jeg har $number æbler!}

In the translation file, the strings can only contain simple variables, optionally surrounded 
by a single pair of curly braces `{}` for disambiguation. The matching between the variables in
the template and the variables in the primary language are positional, whereas the matching 
between the variables in primary and secondary languages are named, so that it is possible to
swap their order if the language requires it.

Calling templates from Java
---------------------------

*To be written*

