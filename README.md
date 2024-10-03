# `yamlcv`: overengineered CV tailoring

Generate tailored CV from data in serialized `yaml` files.

-------------------------------------------------------

Are you someone who has to maintain a cv in different languages, possibly for different fields, and tailors the cv for each application?

Are you the type of person who believes if you do it 10 times and it takes 10 minutes each, then you should spend a few days automating it ([it's actually  always worth the time](https://xkcd.com/1205/), *because it's fun*)?.

Wish you could just store all your experience in some data file, and then generate cvs in `LaTeX` or `HTML` by simply pulling the relevant experience in the relevant language from said file.

Then this may be for you. Or it may not, because maybe [you hate yaml](https://ruudvanasseldonk.com/2023/01/11/the-yaml-document-from-hell).

Warning: This is still pretty much work in progress, since I am the only one using this, I just add features as I need them. But if you want to use it, I'd be happy to help.

## Usage

### the `cv.yaml` file structure

The file you store all your experience in should be a `yaml` file that I will call `cv.yaml` for the sake of this documentation.

#### Links and items

The `cv.yaml` file contains two types of entries: links and items.

A link is a dictionary containing at most 4 keys: `icon`, `text`, `alttext`, and `link`

```yaml
- icon: files/icons/github.svg
  text: tbrugere
  alttext: github
  link: https://github.com/tbrugere
```

In the yaml tree, *any node that is a dictionary containing one of these 4 keys (and not a sub-node of a link or an item) is considered to be a link.*

An item is a dictionary containing at most 4 keys: `date`, `what`, `where`, `precision`

```yaml
- date: 
    begin: 2021
  what: PhD program
  where: UCSD
  precision: Machine learning with a focus on theoretical and mathematical approaches
```

In the yaml tree, *any node that is a dictionary containing one of these 4 keys (and not a sub-node of a link or an item) is considered to be an item*.

**moreover** a string that is not contained in an item or a link is considered to be an item, with the `what` key set to the string

**note on dates:**
all fields in links and items should resolve to strings, except for the `date` field, which can resolve to either:
- a single integer (representing a date)
- a dictionary with the keys `begin` and `end` (representing the date range from `begin` to `end`) and possibly `text`
- a dictionary with a single key `begin` (representing the date range from `begin` to the present) and possibly `text`

The numerical values will be used for sorting cv entries.
If a date contains the field `text`, the contents of that field will be displayed in the cv instead


#### Tags

Tags are used to filter items and links in the cv. They can be added to any item or link as a list of strings under the key `tags`:

```yaml
    - date: 2020
      what: TOEFL
      precision: 109 points out of 120
      tags: [exam, englishlanguage]
```

By using the englishlanguage tag, I can filter out this item for a job that doesn't require English proficiency.

#### The tag tree: outside of items and links

Outside of `items` and `links`, a dictionary that is neither an item nor a link is considered to be a tag tree node. The keys of this dictionary are the tags, and any item or link in the values are tagged with the corresponding tag.

for example

```yaml
education: 
  - date: 
        begin: 2021
    what: PhD program
    where: UCSD
    precision: Machine learning with a focus on theoretical and mathematical approaches
  - date: 2020
    what: TOEFL
    precision: 109 points out of 120
    tags: [exam, englishlanguage]
experience:
  - date: 2021
    what: Internship
    where: Cisco Meraki
```
Here the PhD and TOEFL will be tagged with `education` and the internship with `experience`, so this is equivalent to (ignore the `no-education` and `no-experience` tags for now):

```yaml
- date: 
    begin: 2021
  what: PhD program
  where: UCSD
  precision: Machine learning with a focus on theoretical and mathematical approaches
  tags: [education, no-experience]
- date: 2020
  what: TOEFL
  precision: 109 points out of 120
  tags: [exam, englishlanguage, education, no-experience]
- date: 2021
  what: Internship
  where: Cisco Meraki
  tags: [experience, no-education]
```

Tag nodes **can be nested.** (that's why I have been speaking about trees).

#### The tag tree: inside of item and link fields

tags tree nodes can also be added inside any field of an item or a link. This is very useful for languages. For example:

```yaml
- date: 2021
  what:
    fr: Stage
    en: Internship
  where: Cisco Meraki
```

This is equivalent to creating two different items (ignore the `no-fr` and `no-en` for now)

```yaml
- date: 2021
  what: Stage
  where: Cisco Meraki
  tags: [fr, no-en]
- date: 2021
  what: Stage
  where: Cisco Meraki
  tags: [en, no-fr]
```

#### negative tags, and incompatibilities

**this part might be a bit hard for a first read, but it ensures that tag tree inside items work as expected**

This is where the "overengineered" part starts kicking in. To each tag (for example `experience`) is associated a negative tag, by appending `no-` at the beginning (in our example, `no-experience`).

In a tag tree node, not only do values get tagged with the key, they also get tagged with the negative tags of the other keys of the node.

for example (remember, a lone string is the same as an item with only a `what` key):
```yaml
education:
    - PhD
    - TOEFL
experience:
    - Internship
publications:
    - date: 2021
      what: A paper
```

is equivalent to

```yaml
- what: PhD
  tags: [education, no-experience, no-publications]
- what: TOEFL
  tags: [education, no-experience, no-publications]
- what: Internship
  tags: [experience, no-education, no-publications]
- date: 2021
  what: A paper
  tags: [publications, no-education, no-experience]
```

An entry (item or link) **cannot** have both a tag and its negative tag. Such items will be **automatically filtered**.

For example, the following code:

```yaml
- date: 2021
  what:
    fr: Stage
    en: Internship
  where: Cisco Meraki
  precision:
    fr: Ingénieur Data
    en: Data Engineer
```

should translate to the cartesian product:

```yaml
- date: 2021
  what: Stage
  where: Cisco Meraki
  precision: Ingénieur Data
  tags: [fr, no-en]
- date: 2021
  what: Stage
  where: Cisco Meraki
  precision: Data Engineer
  tags: [fr, no-en, en, no-fr]
- date: 2021
  what: Internship
  where: Cisco Meraki
  precision: Ingénieur Data
  tags: [en, no-fr, fr, no-en]
- date: 2021
  what: Internship
  where: Cisco Meraki
  precision: Data Engineer
  tags: [en, no-fr]
```

but since the incompatible tags are filtered out, we obtain what we want:
```yaml
- date: 2021
  what: Stage
  where: Cisco Meraki
  precision: Ingénieur Data
  tags: [fr, no-en]
- date: 2021
  what: Internship
  where: Cisco Meraki
  precision: Data Engineer
  tags: [en, no-fr]
```

## Command line

### `yamlcv` command

Here are some examples of usage:

```command
$ yamlcv --input cv.yaml --filetype=html --output cv.html
```

```command
$ yamlcv --input cv.yaml --filter-only experience --filetype=html --output cv.html
```

```command
$ yamlcv --input cv.yaml --filter-include fr --filter-exclude obsolete --filetype=latex --output cv.tex
```

full command line help (redacted): 

```command
$ yamlcv --help
yamlc [OPTION]

OPTIONS
       --filetype=FILETYPE
           the output filetype (must be html or latex)

       --filter-exclude=TAG
           will only include elements not tagged with TAG

       --filter-include=TAG
           will  only  include elements not tagged with no-TAG. This is useful
           for languages

       --filter-only=TAG
           will only include elements tagged with TAG

       -i FILENAME, --input=FILENAME (absent=-)
           the yaml input file (default: stdin)

       --latex-style=STYLE (absent=normal)
           the latex style to use

       -o FILENAME, --output=FILENAME (absent=-)
           the output file (default: stdout)

       --ul
           whether to wrap the resulting divs in ul
```

### Pandoc integration

You can also plug yamlcv into pandoc to get a full pipeline of generation and styling.
To do so, you need to 

1. Include `yamlcv` as a filter in pandoc (you can do this by adding `--filter yamlcv` to your pandoc command line)
2. add the `yamlcv_file` metadata entry in your pandoc source file containing the path to the `cv.yaml` file.

you can then include filtered lists of items and links in your pandoc files by 
using code blocks with the `yamlcv` class. The filters are specified inside the code block, like in the following example:
````markdown
```{.yaml .yamlcv}
en: include
education: only
obsolete: exclude
```
````

full templates for pandoc are available in the `templates` directory

## FAQ

### I don’t like yaml

That's not a question. Also, if you really want to use this without yaml, feel free to make a fork that reads another file format. Honestly, if you replace `Yaml.value` with a `Yojson.Basic.t` type in `serialize.ml`, and change calls to `Yaml.parse` in the main module, it should almost work.

### The HTML output is ugly

It needs to be styled with appropriate CSS. There are two examples in the `templates` directory, but you probably need to adapt them to your needs.
