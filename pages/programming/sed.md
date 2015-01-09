# [Sed - An Introduction and Tutorial by Bruce Barnett][1]

Last modified: Thu Jan 8 14:49:06 EST 2015

## [Quick Links][2]

| ----- |
|  Sed Commands |
|  [: label ][3] |  [# comment ][4] |  [{....} Block ][5] |
|  [= ][6]\- print line number |  [a ][7]\- Append  |  [b label ][3] \- Branch |
|  [c - change][8] |  [d ][9] and [D ][10] \- Delete |  [g ][11] and [G ][11] \- Get |
|  [h ][12] and [H ][12] \- Hold |  [i ][13]\- Insert |  [l ][14]\- Look |
|  [n ][10] and [N ][10]\- Next |  [p ][15] and [P ][15]\- Print |  [q ][16]\- Quit |
|  [r filename ][17]\- Read File |  [s/..../..../ ][18]\- Substitute |  [t label ][19]\- Test |
|  [w filename ][20]\- Write Filename |  [x ][21]\- eXchange |  [y/..../..../ ][22]\- Transform |

| ----- |
| Sed Pattern Flags |
| [/g ][23]\- Global |
| [/I ][24]\- Ignore Case |
| [/p ][25]\- Print |
| [/w filename ][26]\- Write Filename |

| ----- |
| Sed Command Line options |
| Short Option (Long option) | Sed version |
| [ -n ][27] | Classic |
| [ -e script][28] | Classic |
| [ -f scriptfile ][29] | Classic |
| [ -e script (--expression=script) ][28] | GNU sed |
| [ -f scriptfile (--file=scriptfile) ][29] | GNU sed |
| [ -h (--help) ][30] | GNU sed |
| [ -n (--quiet --silent) ][27] | GNU sed |
| [ -V (--version) ][31] | GNU sed |
| [ -r (--regexp-extended)][32] | GNU sed |
| [ -i[SUFFIX] (--in-place[=SUFFIX])][33] | GNU sed |
| [ -l N (--line-length=N)][34] | GNU sed |
| [ -b (--binary)][35] | GNU sed |
| [ -s (--separate)][36] | GNU sed |
| [ -z (--null-data)][37] | GNU sed |
| [ -u (--unbuffered)][38] | GNU sed |
| [ (--follow-symlinks)][39] | GNU sed |
| [ (--posix)][40] | GNU sed |
| [ -i SUFFIX][33] | Mac OS X, FreeBSD |
| [ -a][41] | Mac OS X, FreeBSD |
| [ -l][38] | Max OS X, FreeBSD |
| [ -E][42] | Mac OS X, FreeBSD |
| [ -r][42] | FreeBSD |
| [ -I SUFFIX][43] | FreeBSD |

### [Table of Contents][2]

**Note - You can click on the table of contents sections to jump to that section. **

**Then click on the section header of any section to jump back to the table of contents. **

* [The Awful Truth about sed][44]
* [The essential command: s for substitution][18]
* [The slash as a delimiter][45]
* [Using & as the matched string][46]
* [Using 1 to keep part of the pattern][47]
* [Extended Regular Expressions][32]
* [Sed Pattern Flags][48]
    * [/g - Global replacement][23]
    * [Is sed recursive?][49]
    * [/1, /2, etc. Specifying which occurrence][50]
    * [/p - print][25]
    * [Write to a file with /w filename][26]
    * [/I - Ignore Case][24]
    * [Combining substitution flags][51]
* [Arguments and invocation of sed][52]
    * [Multiple commands with -e command][28]
    * [Filenames on the command line][53]
    * [sed -n: no printing][27]
    * [Using 'sed /pattern/'][54]
        * [Using 'sed -n /pattern/p' to duplicate the function of grep][55]
    * [sed -f scriptname][29]
    * [sed in shell scripts][56]
        * [Quoting multiple sed lines in the C shell][57]
        * [Quoting multiple sed lines in the Bourne shell][58]
    * [sed -V][31]
    * [sed -h][30]
    * [A sed interpreter script][59]
    * [Sed Comments][4]
    * [Passing arguments into a sed script][60]
    * [Using sed in a shell here-is document][61]
    * [Multiple commands and order of execution][62]
* [Addresses and Ranges of Text][63]
    * [Restricting to a line number][64]
    * [Patterns][65]
    * [Ranges by line number][66]
    * [Ranges by patterns][67]
* [Delete with d][9]
* [Printing with p][15]
* [Reversing the restriction with !][68]
* [Relationships between d, p, and !][69]
* [The q or quit command][16]
* [Grouping with { and }][5]
* [Operating in a pattern range except for the patterns][70]
* [Writing a file with the 'w' command][20]
* [Reading in a file with the 'r' command][17]
* [SunOS and the # Comment Command][71]
* [Adding, Changing, Inserting new lines][72]
    * [Append a line with 'a'][7]
    * [Insert a line with 'i'][13]
    * [Change a line with 'c'][8]
    * [Leading tabs and spaces in a sed script][73]
    * [Adding more than one line][74]
    * [Adding lines and the pattern space][75]
    * [Address ranges and the above commands][76]
* [Multi-Line Patterns][77]
* [Print line number with =][6]
* [Transform with y][22]
* [Displaying control characters with a l][78]
* [Working with Multiple Lines][10]
    * [Matching three lines with sed][79]
    * [Matching patterns that span multiple lines][80]
    * [Using newlines in sed scripts][81]
    * [The Hold Buffer][82]
    * [Exchange with x][21]
    * [Example of Context Grep][83]
    * [Hold with h or H][12]
    * [Keeping more than one line in the hold buffer][84]
    * [Get with g or G][11]
* [Flow Control][3]
* [Testing with t][19]
* [Debugging with l][14]
* [An alternate way of adding comments][85]
* [The poorly documented ;][86]
* [Passing regular expressions as arguments][87]
* [Inserting binary characters][88]
* [GNU sed Command Line arguments][89]
    * [The -posix argument][40]
    * [The --version argument][90]
    * [The -h Help argument][91]
    * [The -l Line Length Argument][34]
    * [The -s Separate argument][36]
    * [The -i in-place argument][33]
    * [The --follow-symlinks argument][39]
    * [The -b Binary argument][35]
    * [The -r Extended Regular Expression argument][92]
    * [The -u Unbuffered argument][38]
    * [The -z Null Data argument][37]
* [FreeBSD Extensions][93]
    * [-a or delayed open][94]
    * [The -I in-place argument][95]
    * [-E or Extended Regular Expressions][96]
* [Using word boundries][97]
* [Command Summary][98]
* [In Conclusion][99]
* [More References][100]

Copyright 1994, 1995 Bruce Barnett and General Electric Company

Copyright 2001,2005,2007,2011,2013 Bruce Barnett

All rights reserved

You are allowed to print copies of this tutorial for your personal use, and link to this page, but you are not allowed to make electronic copies, or redistribute this tutorial in any form without permission.

Original version written in 1994 and published in the Sun Observer

## [Introduction to Sed][2]

How to use sed, a special editor for modifying files automatically. If you want to write a program to make changes in a file, sed is the tool to use.

There are a few programs that are the real workhorse in the UNIX toolbox. These programs are simple to use for simple applications, yet have a rich set of commands for performing complex actions. Don't let the complex potential of a program keep you from making use of the simpler aspects. I'll start with the simple concepts and introduce the advanced topics later on.   
When I first wrote this (in 1994), most versions of sed did not allow you to place comments inside the script. Lines starting with the '#' characters are comments. Newer versions of sed may support comments at the end of the line as well.

One way to think of this is that the old, "classic" version was the basis of GNU, FreeBSD and Solaris verisons of sed. And to help you understand what I had to work with, here is the [sed(1) manual page from Sun/Oracle][101].

## [The Awful Truth about sed][102]

_Sed_ is the ultimate **s**tream **ed**itor. If that sounds strange, picture a stream flowing through a pipe. Okay, you can't see a stream if it's inside a pipe. That's what I get for attempting a flowing analogy. You want literature, read James Joyce.

Anyhow, _sed_ is a marvelous utility. Unfortunately, most people never learn its real power. The language is very simple, but the documentation is terrible. The Solaris on-line manual pages for _sed_ are five pages long, and two of those pages describe the 34 different errors you can get. A program that spends as much space documenting the errors as it does documenting the language has a serious learning curve.

**Do not fret!** It is not your fault you don't understand _sed_. I will cover _sed_ completely. But I will describe the features in the order that I learned them. I didn't learn everything at once. You don't need to either.

## [The essential command: s for substitution][103]

_Sed_ has several commands, but most people only learn the substitute command: _s_. The substitute command changes all occurrences of the regular expression into a new value. A simple example is changing "day" in the "old" file to "night" in the "new" file:

    sed s/day/night/ new

Or another way (for UNIX beginners),

    sed s/day/night/ old >new

and for those who want to test this:

    echo day | sed s/day/night/

This will output "night".

I didn't put quotes around the argument because this example didn't need them. If you read my earlier tutorial [on quotes][104], you would understand why it doesn't need quotes. However, I recommend you do use quotes. If you have meta-characters in the command, quotes are necessary. And if you aren't sure, it's a good habit, and I will henceforth quote future examples to emphasize the "best practice." Using the strong (single quote) character, that would be:

    sed 's/day/night/' new

I must emphasize that the sed editor changes exactly what you tell it to. So if you executed

    echo Sunday | sed 's/day/night/'

This would output the word "Sunnight" because sed found the string "day" in the input.

Another important concept is that sed is line oriented. Suppose you have the input file:

    one two three, one two three
    four three two one
    one hundred

and you used the command

    sed 's/one/ONE/' [quoting][104]="" [="" expressions.][105].="" that's="" 90%="" effort="" needed="" learn="" command.="" put="" it="" another="" way,="" already="" know="" how="" handle="" most="" frequent="" uses="" _sed._="" a="" ...="" few="" fine="" points="" any="" future="" sed="" expert="" should="" about.="" (you="" just="" finished="" section="" 1.="" 63="" more="" sections="" cover.="" :-)="" oh.="" may="" bookmark="" page,="" ....="" in="" case="" don't="" finish.="" ##="" [the="" slash="" as="" delimiter][106]="" character="" after="" _s_="" delimiter.="" conventionally="" slash,="" because="" what="" _ed_,="" _more_,="" _vi_="" use.="" can="" anything="" want,="" however.="" change="" pathname="" contains="" -="" say="" usr="" local="" bin="" common="" could="" backslash="" quote="" slash:="" 's="" '="" new

Gulp. Some call this a 'Picket Fence' and it's ugly. It is easier to read if you use an underline instead of a slash as a delimiter:

    sed 's_/usr/local/bin_/common/bin_' new

Some people use colons:

    sed 's:/usr/local/bin:/common/bin:' new

Others use the "|" character.

    sed 's|/usr/local/bin|/common/bin|' new

Pick one you like. As long as it's not in the string you are looking for, anything goes. And remember that you need three delimiters. If you get a "Unterminated `s' command" it's because you are missing one of them.

## [Using & as the matched string][107]

Sometimes you want to search for a pattern and add some characters, like parenthesis, around or near the pattern you found. It is easy to do this if you are looking for a particular string:

    sed 's/abc/(abc)/' new

This won't work if you don't know exactly what you will find. How can you put the string you found in the replacement string if you don't know what it is?

The solution requires the special character "&." It corresponds to the pattern found.

    sed 's/[a-z]*/(&)/' new

You can have any number of "&" in the replacement string. You could also double a pattern, e.g. the first number of a line:

    % echo "123 abc" | sed 's/[0-9]*/& &/'
    123 123 abc

Let me slightly amend this example. Sed will match the first string, and make it as greedy as possible. I'll cover that later. If you don't want it to be so greedy (i.e. limit the matching), you need to put restrictions on the match.

The first match for '[0-9]*' is the first character on the line, as this matches zero or more numbers. So if the input was "abc 123" the output would be unchanged (well, except for a space before the letters). A better way to duplicate the number is to make sure it matches a number:

    % echo "123 abc" | sed 's/[0-9][0-9]*/& &/'
    123 123 abc

The string "abc" is unchanged, because it was not matched by the regular expression. If you wanted to eliminate "abc" from the output, you must expand the regular expression to match the rest of the line and explicitly exclude part of the expression using "(", ")" and "1", which is the next topic.

## [Extended Regular Expressions][108]

Let me add a quick comment here because there is another way to write the above script. "[0-9]*" matches zero or more numbers. "[0-9][0-9]*" matches one or more numbers. Another way to do this is to use the "+" meta-character and use the pattern "[0-9]+" as the "+" is a special character when using "extended regular expressions." Extended regular expressions have more power, but sed scripts that treated "+" as a normal character would break. Therefore you must explicitly enable this extension with a command line option.

GNU sed turns this feature on if you use the "-r" command line option. So the above could also be written using

    % echo "123 abc" | sed -r 's/[0-9]+/& &/'
    123 123 abc

Mac OS X and FreeBSD uses [-E][109] instead of [-r][92]. For more information on extended regular expressions, see [Regular Expressions][110] and the [description of the -r command line argument][92]

## [Using 1 to keep part of the pattern][111]

I have already described the use of "(" ")" and "1" in my tutorial on [ regular expressions.][105] To review, the escaped parentheses (that is, parentheses with backslashes before them) remember a substring of the characters matched by the regular expression. You can use this to exclude part of the characters matched by the regular expression. The "1" is the first remembered pattern, and the "2" is the second remembered pattern. Sed has up to nine remembered patterns.

If you wanted to keep the first word of a line, and delete the rest of the line, mark the important part with the parenthesis:

    sed 's/([a-z]*).*/1/'

I should elaborate on this. Regular expressions are greedy, and try to match as much as possible. "[a-z]*" matches zero or more lower case letters, and tries to match as many characters as possible. The ".*" matches zero or more characters after the first match. Since the first one grabs all of the contiguous lower case letters, the second matches anything else. Therefore if you type

    echo abcd123 | sed 's/([a-z]*).*/1/'

This will output "abcd" and delete the numbers.

If you want to switch two words around, you can remember two patterns and change the order around:

    sed 's/([a-z]*) ([a-z]*)/2 1/'

Note the space between the two remembered patterns. This is used to make sure two words are found. However, this will do nothing if a single word is found, or any lines with no letters. You may want to insist that words have at least one letter by using

    sed 's/([a-z][a-z]*) ([a-z][a-z]*)/2 1/'

or by using extended regular expressions (note that '(' and ')' no longer need to have a backslash):

    sed -r 's/([a-z]+) ([a-z]+)/2 1/' # Using GNU sed
    sed -E 's/([a-z]+) ([a-z]+)/2 1/' # Using Apple Mac OS X

The "1" doesn't have to be in the replacement string (in the right hand side). It can be in the pattern you are searching for (in the left hand side). If you want to eliminate duplicated words, you can try:

    sed 's/([a-z]*) 1/1/'

If you want to detect duplicated words, you can use

    sed -n '/([a-z][a-z]*) 1/p'

or with extended regular expressions

    sed -rn '/([a-z]+) 1/p' # GNU sed
    sed -En '/([a-z]+) 1/p' # Mac OS X

This, when used as a filter, will print lines with duplicated words.

The numeric value can have up to nine values: "1" thru "9." If you wanted to reverse the first three characters on a line, you can use

    sed 's/^(.)(.)(.)/321/'

## [Sed Pattern Flags][112]

You can add additional flags after the last delimiter. You might have noticed I used a 'p' at the end of the previous substitute command. I also added the '-n' option. Let me first cover the 'p' and other pattern flags. These flags can specify what happens when a match is found. Let me describe them.

## [/g - Global replacement][113]

Most UNIX utilities work on files, reading a line at a time. _Sed_, by default, is the same way. If you tell it to change a word, it will only change the first occurrence of the word on a line. You may want to make the change on every word on the line instead of the first. For an example, let's place parentheses around words on a line. Instead of using a pattern like "[A-Za-z]*" which won't match words like "won't," we will use a pattern, "[^ ]*," that matches everything except a space. Well, this will also match anything because "*" means **zero or more**. The current version of Solaris's _sed_ (as I wrote this) can get unhappy with patterns like this, and generate errors like "Output line too long" or even run forever. I consider this a bug, and have reported this to Sun. As a work-around, you must avoid matching the null string when using the "g" flag to _sed_. A work-around example is: "[^ ][^ ]*." The following will put parenthesis around the first word:

    sed 's/[^ ]*/(&)/' new

If you want it to make changes for every word, add a "g" after the last delimiter and use the work-around:

    sed 's/[^ ][^ ]*/(&)/g' new

## [Is sed recursive?][114]

_Sed_ only operates on patterns found in the in-coming data. That is, the input line is read, and when a pattern is matched, the modified output is generated, and the **rest** of the input line is scanned. The "s" command will not scan the newly created output. That is, you don't have to worry about expressions like:

    sed 's/loop/loop the loop/g' new

This will not cause an infinite loop. If a second "s" command is executed, it could modify the results of a previous command. I will show you how to execute multiple commands later.

## [/1, /2, etc. Specifying which occurrence][115]

With no flags, the first matched substitution is changed. With the "g" option, all matches are changed. If you want to modify a particular pattern that is not the first one on the line, you could use "(" and ")" to mark each pattern, and use "1" to put the first pattern back unchanged. This next example keeps the first word on the line but deletes the second:

    sed 's/([a-zA-Z]*) ([a-zA-Z]*) /1 /' new

Yuck. There is an easier way to do this. You can add a number after the substitution command to indicate you only want to match that particular pattern. Example:

    sed 's/[a-zA-Z]* //2' new

You can combine a number with the g (global) flag. For instance, if you want to leave the first word alone, but change the second, third, etc. to be DELETED instead, use /2g:

    sed 's/[a-zA-Z]* /DELETED /2g' new

I've heard that combining the number with the g command does not work on The MacOS, and perhaps the FreeSBD version of sed as well.

Don't get /2 and 2 confused. The /2 is used at the end. 2 is used in inside the replacement field.

Note the space after the "*" character. Without the space, _sed_ will run a long, long time. (Note: this bug is probably fixed by now.) This is because the number flag and the "g" flag have the same bug. You should also be able to use the pattern

    sed 's/[^ ]*//2' new

but this also eats CPU. If this works on your computer, and it does on some UNIX systems, you could remove the encrypted password from the password file:

    sed 's/[^:]*//2' /etc/password.new

But this didn't work for me the time I wrote this. Using "[^:][^:]*" as a work-around doesn't help because it won't match an non-existent password, and instead delete the third field, which is the user ID! Instead you have to use the ugly parenthesis:

    sed 's/^([^:]*):[^:]:/1::/'  /etc/password.new

You could also add a character to the first pattern so that it no longer matches the null pattern:

    sed 's/[^:]*:/:/2'  /etc/password.new

The number flag is not restricted to a single digit. It can be any number from 1 to 512. If you wanted to add a colon after the 80th character in each line, you could type:

    sed 's/./&:/80' new

You can also do it the hard way by using 80 dots:

    sed 's/^................................................................................/&:/' new

## [/p - print][116]

By default, _sed_ prints every line. If it makes a substitution, the new text is printed instead of the old one. If you use an optional argument to sed, "sed -n," it will not, by default, print any new lines. I'll cover this and other options later. When the "-n" option is used, the "p" flag will cause the modified line to be printed. Here is one way to duplicate the function of _grep_ with _sed_:

    sed -n 's/pattern/&/p' [later][55]="" ##="" [write="" to="" file="" with="" w="" filename][117]="" there="" one="" more="" flag="" that="" can="" follow="" the="" third="" delimiter.="" it,="" you="" specify="" will="" receive="" modified="" data.="" an="" example="" following,="" which="" write="" all="" lines="" start="" even="" number,="" followed="" by="" space,="" _even_:="" sed="" -n="" 's="" ^[0-9][*02468]="" &="" even'="" ="" gnu="" has="" added="" another="" pattern="" flags="" makes="" match="" case="" insensitive.="" abc,="" etc.:="" '="" abc="" p'="" new

Note that a space after the '/I' and the 'p' (print) command emphasizes that the 'p' is not a modifier of the pattern matching process, , but a command to execute **after** the pattern matching.

## [Combining substitution flags][119]

You can combine flags when it makes sense. Please note that the "w" has to be the last flag. For example the following command works:

    sed -n 's/a/A/2pw /tmp/file' new

Next I will discuss the options to _sed_, and different ways to invoke _sed_.

## [Arguments and invocation of sed][120]

previously, I have only used one substitute command. If you need to make two changes, and you didn't want to read the manual, you could pipe together multiple _sed_ commands:

    sed 's/BEGIN/begin/' new

This used two processes instead of one. A _sed_ guru never uses two processes when one can do.

## [Multiple commands with -e command][121]

One method of combining multiple commands is to use a _-e_ before each command:

    sed -e 's/a/A/' -e 's/b/B/' new

A "-e" isn't needed in the earlier examples because _sed_ knows that there must always be one command. If you give _sed_ one argument, it must be a command, and _sed_ will edit the data read from standard input.

The long argument version is

    sed --expression='s/a/A/' --expression='s/b/B/' new

Also see [Quoting multiple sed lines in the Bourne shell][58]

## [Filenames on the command line][122]

You can specify files on the command line if you wish. If there is more than one argument to _sed_ that does not start with an option, it must be a filename. This next example will count the number of lines in three files that don't begin with a "#:"

    sed 's/^#.*//'  f1 f2 f3 | grep -v '^$' | wc -l

Let's break this down into pieces. The _sed_ substitute command changes every line that starts with a "#" into a blank line. _Grep_ was used to filter out (delete) empty lines. _Wc_ counts the number of lines left. _Sed_ has more commands that make _grep_ unnecessary. And _grep -c_ can replace _wc -l_. I'll discuss how you can duplicate some of _grep_'s functionality later.

Of course you could write the last example using the "-e" option:

    sed -e 's/^#.*//'  f1 f2 f3 | grep -v '^$' | wc -l

There are two other options to _sed._

## [sed -n: no printing][123]

The "-n" option will not print anything unless an explicit request to print is found. I mentioned the "/p" flag to the substitute command as one way to turn printing back on. Let me clarify this. The command

    sed  's/PATTERN/&/p' file

acts like the _cat_ program if PATTERN is not in the file: e.g. nothing is changed. If PATTERN is in the file, then each line that has this is printed twice. Add the "-n" option and the example acts like grep:

    sed -n 's/PATTERN/&/p' file

Nothing is printed, except those lines with PATTERN included.

The long argument of the -n command is either

    sed --quiet 's/PATTERN/&/p' file

or

    sed --silent 's/PATTERN/&/p' file

## [Using 'sed /pattern/'][124]

_Sed_ has the ability to specify which lines are to be examined and/or modified, by specifying [addresses][125] before the command. I will just describe the simplest version for now - the /PATTERN/ address. When used, only lines that match the pattern are given the command after the address. Briefly, when used with the /p flag, matching lines are printed twice:

    sed '/PATTERN/p' file

And of course PATTERN is any regular expression.

Please note that if you do not include a command, such as the "p" for print, you will get an error. When I type

    echo abc | sed '/a/'

I get the error

    sed: -e expression #1, char 3: missing command

Also, you don't need to, but I recommend that you place a space after the pattern and the command. This will help you distinquish between flags that modify the pattern matching, and commands to execute after the pattern is matched. Therefore I recommend this style:

    sed '/PATTERN/ p' file

## [Using 'sed -n /pattern/p' to duplicate the function of grep][126]

If you want to duplicate the functionality of grep, combine the -n (noprint) option with the /p print flag:

    sed -n '/PATTERN/p' file

## [sed -f scriptname][127]

If you have a large number of _sed_ commands, you can put them into a file and use

    sed -f sedscript new

where _sedscript_ could look like this:

    # sed comment - This script changes lower case vowels to upper case
    s/a/A/g
    s/e/E/g
    s/i/I/g
    s/o/O/g
    s/u/U/g

When there are several commands in one file, each command must be on a separate line.

The long argument version is

    sed --file=sedscript new

Also see [here on writing a script that executes sed directly][59]

## [sed in shell scripts][128]

If you have many commands and they won't fit neatly on one line, you can break up the line using a backslash:

    sed -e 's/a/A/g'
        -e 's/e/E/g'
        -e 's/i/I/g'
        -e 's/o/O/g'
        -e 's/u/U/g'  new

## [Quoting multiple sed lines in the C shell][129]

You can have a large, multi-line _sed_ script in the C shell, but you must tell the C shell that the quote is continued across several lines. This is done by placing a backslash at the end of each line:

    #!/bin/csh -f
    sed 's/a/A/g
    s/e/E/g
    s/i/I/g
    s/o/O/g
    s/u/U/g'  new

## [Quoting multiple sed lines in the Bourne shell][130]

The Bourne shell makes this easier as a quote can cover several lines:

    #!/bin/sh
    sed '
    s/a/A/g
    s/e/E/g
    s/i/I/g
    s/o/O/g
    s/u/U/g'  new

## [sed -V][131]

The -V option will print the version of sed you are using. The long argument of the command is

    sed --version

## [sed -h][132]

The -h option will print a summary of the sed commands. The long argument of the command is

    sed --help

## [A sed interpreter script][133]

Another way of executing _sed_ is to use an interpreter script. Create a file that contains:   
  
#!/bin/sed -f  
s/a/A/g  
s/e/E/g  
s/i/I/g  
s/o/O/g  
s/u/U/g  

  
Click here to get file: [CapVowel.sed][134]  
If this script was stored in a file with the name "CapVowel" and was executable, you could use it with the simple command:

    CapVowel new

## [Comments][135]

_Sed_ comments are lines where the first non-white character is a "#." On many systems, _sed_ can have only one comment, and it must be the first line of the script. On the Sun (1988 when I wrote this), you can have several comment lines anywhere in the script. Modern versions of Sed support this. If the first line contains exactly "#n" then this does the same thing as the "-n" option: turning off printing by default. This could not done with a _sed_ interpreter script, because the first line must start with "#!/bin/sed -f" as I think "#!/bin/sed -nf" generated an error. It worked when I first wrote this (2008). Note that "#!/bin/sed -fn" does not work because sed thinks the filename of the script is "n". However,

    "#!/bin/sed -nf"

does work.

## [Passing arguments into a sed script][136]

Passing a word into a shell script that calls _sed_ is easy if you remembered [my tutorial on the UNIX quoting mechanism.][104] To review, you use the single quotes to turn quoting on and off. A simple shell script that uses _sed_ to emulate grep is:   
  
#!/bin/sh  
sed -n 's/'$1'/&/p'  
  
However - there is a problem with this script. If you have a space as an argument, the script would cause a syntax error A better version would protect from this happening:   
  

    #!/bin/sh
    sed -n 's/'"$1"'/&/p'

  
Click here to get file: [sedgrep.sed][137]  
If this was stored in a file called _sedgrep_, you could type

    sedgrep '[A-Z][A-Z]' ="" you="" can="" use="" _sed_="" prompt="" user="" for="" some="" parameters="" and="" then="" create="" file="" with="" those="" filled="" in.="" could="" dummy="" values="" placed="" inside="" it,="" change="" values.="" simpler="" way="" is="" "here="" is"="" document,="" which="" uses="" part="" of="" script="" if="" it="" were="" standard="" input:="" #!="" bin="" sh="" echo="" -n="" 'what="" value?="" '="" read="" value="" 's="" xyz="" '$value'="" <[tutorial="" on="" quotation="" marks][139].="" click="" here="" get="" file:="" [sed_hereis.sed][140]="" [multiple="" commands="" order="" execution][141]="" we="" explore="" more="" _sed_,="" become="" complex,="" actual="" sequence="" confusing.="" it's="" really="" quite="" simple.="" each="" command,="" specified="" by="" user,="" has="" chance="" operate="" input="" line.="" after="" substitutions="" are="" made,="" command="" line,="" may="" been="" modified="" earlier="" commands.="" ever="" question,="" best="" learn="" happen="" small="" doesn't="" work,="" make="" simpler.="" having="" problems="" getting="" working,="" break="" up="" into="" two="" smaller="" scripts="" pipe="" together.="" [addresses="" ranges="" text][142]="" only="" learned="" one="" see="" how="" powerful="" is.="" all="" doing="" _grep_="" substitute.="" is,="" substitute="" treating="" itself,="" caring="" about="" nearby="" lines.="" ability="" restrict="" operation="" certain="" restrictions="" might="" :="" *="" specifying="" its="" number.="" range="" lines="" containing="" pattern.="" from="" beginning="" regular="" expression="" end="" file.="" between="" expressions.="" do="" more.="" every="" proceeded="" address,="" or="" restriction="" above="" examples.="" address="" immediately="" precedes="" command:="" _restriction_="" _command_="" [restricting="" number][143]="" simplest="" wanted="" delete="" first="" number="" 3,="" just="" add="" "3"="" before="" '3="" s="" [0-9][0-9]*="" new

## [Patterns][144]

Many UNIX utilities like _vi_ and _more_ use a slash to search for a regular expression. _Sed_ uses the same convention, provided you terminate the expression with a slash. To delete the first number on all lines that start with a "#," use:

    sed '/^#/ s/[0-9][0-9]*//'

I placed a space after the "/_expression_/" so it is easier to read. It isn't necessary, but without it the command is harder to fathom. _Sed_ does provide a few extra options when specifying regular expressions. But I'll discuss those later. If the expression starts with a backslash, the next character is the delimiter. To use a comma instead of a slash, use:

    sed ',^#, s/[0-9][0-9]*//'

The main advantage of this feature is searching for slashes. Suppose you wanted to search for the string "/usr/local/bin" and you wanted to change it for "/common/all/bin." You could use the backslash to escape the slash:

    sed '//usr/local/bin/ s//usr/local//common/all/'

It would be easier to follow if you used an underline instead of a slash as a search. This example uses the underline in both the search command and the substitute command:

    sed '_/usr/local/bin_ s_/usr/local_/common/all_'

This illustrates why _sed_ scripts get the reputation for obscurity. I could be perverse and show you the example that will search for all lines that start with a "g," and change each "g" on that line to an "s:"

    sed '/^g/s/g/s/g'

Adding a space and using an underscore after the substitute command makes this **much** easier to read:

    sed '/^g/ s_g_s_g'

Er, I take that back. It's hopeless. There is a lesson here: Use comments liberally in a _sed_ script. You may have to remove the comments to run the script under a different (older) operating system, but you now know how to write a _sed_ script to do that very easily! Comments are a Good Thing. You may have understood the script perfectly when you wrote it. But six months from now it could look like modem noise. And if you don't understand that reference, imagine an 8-month-old child typing on a computer.

## [Ranges by line number][145]

You can specify a range on line numbers by inserting a comma between the numbers. To restrict a substitution to the first 100 lines, you can use:

    sed '1,100 s/A/a/'

If you know exactly how many lines are in a file, you can explicitly state that number to perform the substitution on the rest of the file. In this case, assume you used _wc_ to find out there are 532 lines in the file:

    sed '101,532 s/A/a/'

An easier way is to use the special character "$," which means the last line in the file.

    sed '101,$ s/A/a/'

The "$" is one of those conventions that mean "last" in utilities like _cat -e_, _vi_, and _ed_. "cat -e" Line numbers are cumulative if several files are edited. That is,

    sed '200,300 s/A/a/' f1 f2 f3 >new

is the same as

    cat f1 f2 f3 | sed '200,300 s/A/a/' >new

## [Ranges by patterns][146]

You can specify two regular expressions as the range. Assuming a "#" starts a comment, you can search for a keyword, remove all comments until you see the second keyword. In this case the two keywords are "start" and "stop:"

    sed '/start/,/stop/ s/#.*//'

The first pattern turns on a flag that tells _sed_ to perform the substitute command on every line. The second pattern turns off the flag. If the "start" and "stop" pattern occurs twice, the substitution is done both times. If the "stop" pattern is missing, the flag is never turned off, and the substitution will be performed on every line until the end of the file.

You should know that if the "start" pattern is found, the substitution occurs on the same line that contains "start." This turns on a switch, which is line oriented. That is, the next line is read and the substitute command is checked. If it contains "stop" the switch is turned off. Switches are line oriented, and not word oriented.

You can combine line numbers and regular expressions. This example will remove comments from the beginning of the file until it finds the keyword "start:"

    sed -e '1,/start/ s/#.*//'

This example will remove comments everywhere except the lines **between** the two keywords:

    sed -e '1,/start/ s/#.*//' -e '/stop/,$ s/#.*//'

The last example has a range that overlaps the "/start/,/stop/" range, as both ranges operate on the lines that contain the keywords. I will show you later how to restrict a command up to, **but not including** the line containing the specified pattern. It is in [Operating in a pattern range except for the patterns][70] But I have to cover some more basic principles.

Before I start discussing the various commands, I should explain that some commands cannot operate on a range of lines. I will let you know when I mention the commands. In this next section I will describe three commands, one of which cannot operate on a range.

## [Delete with d][147]

Using ranges can be confusing, so you should expect to do some experimentation when you are trying out a new script. A useful command deletes every line that matches the restriction: "d." If you want to look at the first 10 lines of a file, you can use:

    sed '11,$ d' [sed_tail.sh][148]="" range="" for="" deletions="" be="" regular="" expressions="" pairs="" mark="" begin="" end="" operation.="" or="" it="" single="" expression.="" deleting="" all="" that="" with="" "#"="" easy:="" '="" ^#="" removing="" comments="" takes="" two="" commands.="" removes="" every="" character="" second="" deletes="" lines:="" -e="" 's="" #.*="" third="" one="" should="" added="" remove="" blanks="" tabs="" immediately="" before="" line:="" [="" ^i]*$="" "^i"="" _ctrl-i_="" tab="" character.="" would="" have="" explicitly="" type="" tab.="" note="" order="" operations="" above,="" very="" good="" reason.="" middle="" white="" space="" characters="" them.="" therefore="" are="" removed="" potentially="" leaving="" were="" comment.="" command="" trailing="" blanks,="" so="" now="" converted="" empty="" together,="" three="" commands="" containing="" only="" comments,="" spaces.="" this="" demonstrates="" pattern="" _sed_="" uses="" operate="" on="" line.="" actual="" operation="" is:="" :="" *="" copy="" input="" line="" into="" space.="" apply="" space,="" address="" restriction="" true.="" repeat="" next="" expression,="" again="" operating="" when="" performed,="" write="" out="" read="" ##="" [printing="" p][149]="" another="" useful="" print="" command:="" "p."="" wasn't="" started="" an="" "-n"="" option,="" "p"="" will="" input.="" 'p'="" wanted="" double="" p'="" adding="" option="" turns="" printing="" unless="" request="" it.="" way="" duplicating="" _head_'s="" functionality="" want.="" example="" prints="" -n="" '1,10="" act="" _grep_="" by="" combining="" operator="" match="" expression:="" same="" as:="" grep="" [reversing="" !][150]="" sometimes="" need="" perform="" action="" except="" those="" outside="" addresses.="" "!"="" character,="" often="" means="" _not_="" unix="" utilities,="" inverts="" restriction.="" remember="" acts="" "-v"="" don't="" contain="" pattern.="" do="" !p'="" <="" tmp="" b="" [relationships="" between="" d,="" p,="" !][151]="" as="" may="" noticed,="" there="" several="" ways="" solve="" problem="" _sed_.="" because="" _print_="" _delete_="" opposite="" functions,="" appears="" "!p"="" "d,"="" while="" "!d"="" i="" test="" this,="" created="" 20="" file,="" tried="" different="" combination.="" following="" table,="" shows="" results,="" difference:="" -----="" relations="" !="" results="" ------="" -------="" ---------------------------------------------------="" 1,10="" p="" 11,$="" !p="" !d="" d="" nothing="" printed="" twice,="" then="" once="" once,="" twice="" table="" identical:="" '11,$="" !d'="" also="" "inverts"="" range,="" other="" [the="" q="" quit="" command][152]="" more="" simple="" restrict="" changes="" set="" "q"="" quit.="" head="" '11="" q'="" quits="" eleventh="" reached.="" most="" wish="" abort="" editing="" after="" some="" condition="" does="" not="" take="" obviously="" cannot="" times.="" instead="" '1="" '10="" correct.="" [grouping="" {="" }][153]="" curly="" braces,="" "{"="" "},"="" used="" group="" hardly="" worth="" buildup.="" prose="" solution="" just="" matching="" squiggles.="" well,="" complication.="" since="" each="" must="" its="" own="" braces="" nested="" separate="" previously,="" showed="" how="" starting="" "#."="" removal="" special="" "begin"="" "end"="" key="" words,="" could="" #-type="" 'begin'="" 'end'="" words.="" ,="" s="" }="" [sed_begin_end.sh][154]="" these="" nested,="" allow="" combine="" ranges.="" before,="" but="" limit="" change="" 100="" 1,100="" [sed_begin_end1.sh][155]="" place="" braces.="" address,="" **except**="" reserved="" words:="" !{="" [sed_begin_end2.sh][156]="" [operating="" patterns][157]="" mentioned="" substitute="" changing="" "old"="" "new"="" pattern:="" old="" new="" use="" grouping:="" think="" makes="" code="" clearer="" understand,="" easier="" modify,="" see="" below.="" did="" make="" any="" where="" word="" occurred,="" add="" skip="" over="" n="" has="" however,="" skipping="" trickier.="" method="" engine="" stop="" skips="" well.="" using="" [writing="" 'w'="" command][158]="" even="" (and="" followed="" space):="" ^[0-9][*02468]="" &="" w="" even'="" "&"="" replacement="" part="" substitution="" changed.="" simpler="" "w"="" syntax="" flag="" follow="" anything="" else="" considered="" name.="" limitation="" flag:="" files="" opened="" [reading="" 'r'="" command][159]="" reading="" files.="" '$r="" end'="" out

will append the file "end" at the end of the file (address "$)." The following will insert a file after the line with the word "INCLUDE:"

    sed '/INCLUDE/ r file' out

You can use the curly braces to delete the line having the "INCLUDE" command on it:   
  

    #!/bin/sh
    sed '/INCLUDE/ {
        r file
        d
    }'

  
Click here to get file: [sed_include.sh][160]  

The order of the delete command "d" and the read file command "r" is important. Change the order and it will not work. There are two subtle actions that prevent this from working. The first is the "r" command writes the file to the output stream. The file is not inserted into the pattern space, and therefore cannot be modified by any command. Therefore the delete command does not affect the data read from the file.

The other subtlety is the "d" command deletes the current data in the pattern space. Once all of the data is deleted, it does make sense that no other action will be attempted. Therefore a "d" command executed in a curly brace also aborts all further actions. As an example, the substitute command below is never executed:   
  

    #!/bin/sh
    # this example is WRONG
    sed -e '1 {
        d
        s/.*//
    }'

  
Click here to get file: [sed_bad_example.sh][161]  

The earlier example is a crude version of the C preprocessor program. The file that is included has a predetermined name. It would be nice if _sed_ allowed a variable (e.g "1" ) instead of a fixed file name. Alas, _sed_ doesn't have this ability. You could work around this limitation by creating _sed_ commands on the fly, or by using shell quotes to pass variables into the _sed_ script. Suppose you wanted to create a command that would include a file like _cpp_, but the filename is an argument to the script. An example of this script is:

    % include 'sys/param.h' file.c.new

A shell script to do this would be:

    #!/bin/sh
    # watch out for a '/' in the parameter
    # use alternate search delimiter
    sed -e '_#INCLUDE <'"$1"'>_{
        r '"$1"'
        d
    }'

Let me elaborate. If you had a file that contains

    Test first file
    #INCLUDE 
    Test second file
    #INCLUDE 

you could use the command

    sed_include1.sh file1[sed_include1.sh][162]="" ##="" [sunos="" and="" #="" comment="" command][163]="" as="" we="" dig="" deeper="" into="" _sed_,="" comments="" will="" make="" commands="" easier="" follow.="" older="" versions="" of="" _sed_="" only="" allow="" one="" line="" a="" comment,="" it="" must="" be="" first="" line.="" sunos="" (and="" gnu's="" sed)="" allows="" more="" than="" these="" don't="" have="" first.="" last="" example="" could="" be:="" #!="" bin="" sh="" watch="" out="" for="" '="" in="" parameter="" use="" alternate="" search="" delimiter="" sed="" -e="" '_#include="" <'"$1"'="">_{

        # read the file
        r '"$1"'

        # delete any characters in the pattern space
        # and read the next line in
        d
    }'

  
Click here to get file: [sed_include2.sh][164]  

## [Adding, Changing, Inserting new lines][165]

_Sed_ has three commands used to add new lines to the output stream. Because an entire line is added, the new line is on a line by itself to emphasize this. There is no option, an entire line is used, and it must be on its own line. If you are familiar with many UNIX utilities, you would expect _sed_ to use a similar convention: lines are continued by ending the previous line with a "". The syntax to these commands is finicky, like the "r" and "w" commands.

## [Append a line with 'a'][166]

The "a" command appends a line after the range or pattern. This example will add a line after every line with "WORD:"   
  

    #!/bin/sh
    sed '
    /WORD/ a
    Add this line after every line with WORD
    '

  
Click here to get file: [sed_add_line_after_word.sh][167]  

You could eliminate two lines in the shell script if you wish:   
  

    #!/bin/sh
    sed '/WORD/ a
    Add this line after every line with WORD'

  
Click here to get file: [sed_add_line_after_word1.sh][168]  

I prefer the first form because it's easier to add a new command by adding a new line and because the intent is clearer. There must not be a space after the "".

## [Insert a line with 'i'][169]

You can insert a new line before the pattern with the "i" command:   
  

    #!/bin/sh
    sed '
    /WORD/ i
    Add this line before every line with WORD
    '

  
Click here to get file: [sed_add_line_before_word.sh][170]  

## [Change a line with 'c'][171]

You can change the current line with a new line.   
  

    #!/bin/sh
    sed '
    /WORD/ c
    Replace the current line with the line
    '

  
Click here to get file: [sed_change_line.sh][172]  

A "d" command followed by a "a" command won't work, as I discussed earlier. The "d" command would terminate the current actions. You can combine all three actions using curly braces:   
  

    #!/bin/sh
    sed '
    /WORD/ {
    i
    Add this line before
    a
    Add this line after
    c
    Change the line to this one
    }'

  
Click here to get file: [sed_insert_append_change.sh][173]  

## [Leading tabs and spaces in a sed script][174]

_Sed_ ignores leading tabs and spaces in all commands. However these white space characters may or may not be ignored if they start the text following a "a," "c" or "i" command. In SunOS, both "features" are available. The Berkeley (and Linux) style sed is in /usr/bin, and the AT&T version (System V) is in /usr/5bin/.

To elaborate, the **/usr/bin/sed** command retains white space, while the **/usr/5bin/sed** strips off leading spaces. If you want to keep leading spaces, and not care about which version of _sed_ you are using, put a "" as the first character of the line:

    #!/bin/sh
    sed '
        a
        This line starts with a tab
    '

## [Adding more than one line][175]

All three commands will allow you to add more than one line. Just end each line with a ":"

    #!/bin/sh
    sed '
    /WORD/ a
    Add this line
    This line
    And this line
    '

## [Adding lines and the pattern space][176]

I have mentioned the pattern space before. Most commands operate on the pattern space, and subsequent commands may act on the results of the last modification. The three previous commands, like the read file command, add the new lines to the output stream, bypassing the pattern space.

## [Address ranges and the above commands][177]

You may remember that earlier I warned you that some commands can take a range of lines, and others cannot. To be precise, the commands "a," "i," "r," and "q" will not take a range like "1,100" or "/begin/,/end/." The documentation states that the read command can take a range, but I got an error when I tried this. The "c" or change command allows this, and it will let you change several lines into one:

    #!/bin/sh
    sed '
    /begin/,/end/ c
    ***DELETED***
    '

If you need to do this, you can use the curly braces, as that will let you perform the operation on every line:

    #!/bin/sh
    # add a blank line after every line
    sed '1,$ {
        a

    }'

## [Multi-Line Patterns][178]

Most UNIX utilities are line oriented. Regular expressions are line oriented. Searching for patterns that covers more than one line is not an easy task. (Hint: It will be very shortly.)

_Sed_ reads in a line of text, performs commands which may modify the line, and outputs modification if desired. The main loop of a _sed_ script looks like this:

1. The next line is read from the input file and places it in the pattern space. If the end of file is found, and if there are additional files to read, the current file is closed, the next file is opened, and the first line of the new file is placed into the pattern space.
2. The line count is incremented by one. Opening a new file does not reset this number.
3. Each _sed_ command is examined. If there is a restriction placed on the command, and the current line in the pattern space meets that restriction, the command is executed. Some commands, like "n" or "d" cause _sed_ to go to the top of the loop. The "q" command causes _sed_ to stop. Otherwise the next command is examined.
4. After all of the commands are examined, the pattern space is output unless _sed_ has the optional "-n" argument.

The restriction before the command determines if the command is executed. If the restriction is a pattern, and the operation is the delete command, then the following will delete all lines that have the pattern:

    /PATTERN/ d

If the restriction is a pair of numbers, then the deletion will happen if the line number is equal to the first number or greater than the first number and less than or equal to the last number:

    10,20 d

If the restriction is a pair of patterns, there is a variable that is kept for each of these pairs. If the variable is false and the first pattern is found, the variable is made true. If the variable is true, the command is executed. If the variable is true, and the last pattern is on the line, after the command is executed the variable is turned off:

    /begin/,/end/ d

Whew! That was a mouthful. If you have read carefully up to here, you should have breezed through this. You may want to refer back, because I covered several subtle points. My choice of words was deliberate. It covers some unusual cases, like:

    # what happens if the second number
    # is less than the first number?
    sed -n '20,1 p' file

and

    # generate a 10 line file with line numbers
    # and see what happens when two patterns overlap
    yes | head -10 | cat -n |
    sed -n -e '/1/,/7/ p' -e '/5/,/9/ p'

Enough mental punishment. Here is another review, this time in a table format. Assume the input file contains the following lines:

    AB
    CD
    EF
    GH
    IJ

When _sed_ starts up, the first line is placed in the pattern space. The next line is "CD." The operations of the "n," "d," and "p" commands can be summarized as:

| ----- |
|  Pattern Space |  Next Input |  Command |  Output |  New Pattern Space |  New Text Input |
|  AB |  CD |  n |   |  CD |  EF |
|  AB |  CD |  d |  - |  CD |  EF |
|  AB |  CD |  p |  AB |  CD |  EF |

The "n" command may or may not generate output depending upon the existence of the "-n" flag.

That review is a little easier to follow, isn't it? Before I jump into multi-line patterns, I wanted to cover three more commands:

## [Print line number with =][179]

The "=" command prints the current line number to standard output. One way to find out the line numbers that contain a pattern is to use:

    # add line numbers first,
    # then use grep,
    # then just print the number
    cat -n file | grep 'PATTERN' | awk '{print $1}'

The _sed_ solution is:

    sed -n '/PATTERN/ =' file

Earlier I used the following to find the number of lines in a file

    #!/bin/sh
    lines=`wc -l file | awk '{print $1}' `

Using the "=" command can simplify this:

    #!/bin/sh
    lines=`sed -n '$=' file `

The "=" command only accepts one address, so if you want to print the number for a range of lines, you must use the curly braces:

    #!/bin/sh
    # Just print the line numbers
    sed -n '/begin/,/end/ {
    =
    d
    }' file

Since the "=" command only prints to standard output, you cannot print the line number on the same line as the pattern. You need to edit multi-line patterns to do this.

## [Transform with y][180]

If you wanted to change a word from lower case to upper case, you could write 26 character substitutions, converting "a" to "A," etc. _Sed_ has a command that operates like the _tr_ program. It is called the "y" command. For instance, to change the letters "a" through "f" into their upper case form, use:

    sed 'y/abcdef/ABCDEF/' file

Here's a sed example that convers all uppercase letters to lowercase letters, like the tr command:

    sed 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/' lowercase

If you wanted to convert a line that contained a hexadecimal number (e.g. 0x1aff) to upper case (0x1AFF), you could use:

    sed '/0x[0-9a-zA-Z]*/ y/abcdef/ABCDEF' file

This works fine if there are only numbers in the file. If you wanted to change the second word in a line to upper case, and you are using classic sed, you are out of luck - unless you use multi-line editing. (Hey - I think there is some sort of theme here!)

However, GNU sed has a uppercase and lowercase extension.

## [Displaying control characters with a l][181]

The "l" command prints the current pattern space. It is therefore useful in debugging _sed_ scripts. It also converts unprintable characters into printing characters by outputting the value in octal preceded by a "" character. I found it useful to print out the current pattern space, while probing the subtleties of _sed_.

## [Working with Multiple Lines][182]

There are three new commands used in multiple-line patterns: "N," "D," and "P." I will explain their relation to the matching "n," "d," and "p" single-line commands.

The "n" command will print out the current pattern space (unless the "-n" flag is used), empty the current pattern space, and read in the next line of input. The "N" command does **not** print out the current pattern space and does **not** empty the pattern space. It reads in the next line, but appends a new line character along with the input line itself to the pattern space.

The "d" command deletes the current pattern space, reads in the next line, puts the new line into the pattern space, and aborts the current command, and starts execution at the first _sed_ command. This is called starting a new "cycle." The "D" command deletes the first portion of the pattern space, up to the new line character, leaving the rest of the pattern alone. Like "d," it stops the current command and starts the command cycle over again. However, it will not print the current pattern space. You must print it yourself, a step earlier. If the "D" command is executed with a group of other commands in a curly brace, commands after the "D" command are ignored. The next group of _sed_ commands is executed, unless the pattern space is emptied. If this happens, the cycle is started from the top and a new line is read.

The "p" command prints the entire pattern space. The "P" command only prints the first part of the pattern space, up to the NEWLINE character. Neither the "p" nor the "P" command changes the patterns space.

Some examples might demonstrate "N" by itself isn't very useful. the filter

    sed -e 'N'

doesn't modify the input stream. Instead, it combines the first and second line, then prints them, combines the third and fourth line, and prints them, etc. It does allow you to use a new "anchor" character: "n." This matches the new line character that separates multiple lines in the pattern space. If you wanted to search for a line that ended with the character "#," and append the next line to it, you could use

    #!/bin/sh
    sed '
    # look for a "#" at the end of the line
    /#$/ {
    # Found one - now read in the next line
        N
    # delete the "#" and the new line character,
        s/#n//
    }' file

You could search for two lines containing "ONE" and "TWO" and only print out the two consecutive lines:

    #!/bin/sh
    sed -n '
    /ONE/ {
    # found "ONE" - read in next line
        N
    # look for "TWO" on the second line
    # and print if there.
        /n.*TWO/ p
    }' file

The next example would delete everything between "ONE" and "TWO:"

    #!/bin/sh
    sed '
    /ONE/ {
    # append a line
        N
    # search for TWO on the second line
        /n.*TWO/ {
    # found it - now edit making one line
            s/ONE.*n.*TWO/ONE TWO/
        }
    }' file

## [Matching three lines with sed][183]

You can match multiple lines in searches.

Here is a way to look for the string "skip3", and if found, delete that line and the next two lines.

    #!/bin/sh
    sed '/skip3/ {
               N
               N
               s/skip3n.*n.*/# 3 lines deleted/
    }'

Note that it doesn't matter what the next two lines are. If you wanted to match 3 particular lines, it's a little more work.

This script looks for three lines, where the first line contains "one", the second contained "two" and the third contains "three", and if found, replace them with the string "1+2+3":

    #!/bin/sh
    sed '
    /one/ {
          N
          /two/ {
                N
                /three/ {
                        N
                        s/onentwonthree/1+2+3/
                        }
                }
          }
    '

## [Matching patterns that span multiple lines][184]

You can either search for a particular pattern on two consecutive lines, or you can search for two consecutive words that may be split on a line boundary. The next example will look for two words which are either on the same line or one is on the end of a line and the second is on the beginning of the next line. If found, the first word is deleted:

    #!/bin/sh
    sed '
    /ONE/ {
    # append a line
        N
    # "ONE TWO" on same line
        s/ONE TWO/TWO/
    # "ONE
    # TWO" on two consecutive lines
        s/ONEnTWO/TWO/
    }' file

Let's use the  
"D" command, and if we find a line containing  
"TWO" immediately after a line containing  
"ONE," then delete the first line:  
  
  

    #!/bin/sh
    sed '
    /ONE/ {
    # append a line
        N
    # if TWO found, delete the first line
        /n.*TWO/ D
    }' file

  
Click here to get file: [sed_delete_line_after_word.sh][185]  

If we wanted to print the first line instead of deleting it, and not print every other line, change the "D" to a "P" and add a "-n" as an argument to _sed_:   
  

    #!/bin/sh
    sed -n '
    # by default - do not print anything
    /ONE/ {
    # append a line
        N
    # if TWO found, print the first line
        /n.*TWO/ P
    }' file

  
Click here to get file: [sed_print_line_after_word.sh][186]  

It is very common to combine all three multi-line commands. The typical order is "N," "P" and lastly "D." This one will delete everything between "ONE" and "TWO" if they are on one or two consecutive lines:   
  

    #!/bin/sh
    sed '
    /ONE/ {
    # append the next line
        N
    # look for "ONE" followed by "TWO"
        /ONE.*TWO/ {
    #   delete everything between
            s/ONE.*TWO/ONE TWO/
    #   print
            P
    #   then delete the first line
            D
        }
    }' file

  
Click here to get file: [sed_delete_between_two_words.sh][187]  

Earlier I talked about the "=" command, and using it to add line numbers to a file. You can use two invocations of _sed_ to do this (although it is possible to do it with one, but that must wait until next section). The first _sed_ command will output a line number on one line, and then print the line on the next line. The second invocation of _sed_ will merge the two lines together:   
  

    #!/bin/sh
    sed '=' file |
    sed '{
        N
        s/n/ /
    }'

  
Click here to get file: [sed_merge_two_lines.sh][188]  

If you find it necessary, you can break one line into two lines, edit them, and merge them together again. As an example, if you had a file that had a hexadecimal number followed by a word, and you wanted to convert the first word to all upper case, you can use the "y" command, but you must first split the line into two lines, change one of the two, and merge them together. That is, a line containing

    0x1fff table2

will be changed into two lines:

    0x1fff
    table2

and the first line will be converted into upper case. I will use _tr_ to convert the space into a new line, and then use _sed_ to do the rest. The command would be

    ./sed_split