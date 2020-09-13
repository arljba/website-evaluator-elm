# website-evaluator-elm
This project connects to diffrent APIs which provide data deemed relevant to a website. The Data is processed and rendered.The project can be tested here. 

![websitelem](https://user-images.githubusercontent.com/33658992/93005996-4530b000-f557-11ea-9077-107a188d4d55.PNG)

---

## Table of Contents

- [Manual](#manual)
- [Documentation](#documentation)

---

## Manual

### Step 1

First define a website which should be evaluated. The url of the website has to something like _http://google.com_ (_google.com_ would not be a valid url).

![grafik](https://user-images.githubusercontent.com/33658992/93006131-c3da1d00-f558-11ea-9add-846b907dd361.png)

### Step 2

In the second step the apis which should be called need to be selected. To select an api the active switch needs to be toogled.

![website2](https://user-images.githubusercontent.com/33658992/93006163-3a771a80-f559-11ea-92aa-cd9e589aec8a.PNG)

### Step 3
After that simply hit the check button. 

![grafik](https://user-images.githubusercontent.com/33658992/93006198-9e99de80-f559-11ea-98a7-bd256d065ef9.png)

### Step 4
Next you need to wait untill the data is fetched and displayed after the status changes to sucess the result can be viewed.

![grafik](https://user-images.githubusercontent.com/33658992/93006267-5e872b80-f55a-11ea-9602-656a6f41146b.png)

### Step 5
To view the result of a given task just hit the arrowbutton on the far right 

![website3](https://user-images.githubusercontent.com/33658992/93006370-96db3980-f55b-11ea-9341-e13b8de8a139.PNG)

---
## Documentation
### Intent
The intent of the project is to provide valuabel information about a website which couldnt be retrieved arbitrarely. Alot of enterprieses use sophisticated tools to monitor theyr website and get information like how many people are visiting what pages and how a user traverses the site. Obviously these are informations which cant be retrieved if not for being the owner of said website.This project rather focuses on gathering information which are available to the whole internet. In the following section the diffrent APIs that are being used for this purpose are being listed.

### APIs
#### WhoIsApi
This api basicly functions the same as the whois terminal comand. Whois is a protocol with which information on internet domains and ip addresses and their owners can be queried from a distributed database system. The api does the same and returns the queried result in the xml fomat.
This api might not work with specific country domains (e.g .de).
#### GooglePageSpeed
This api checks how fast content on a website can be served to a user. Additionaly it returns an audit where tips are given how the website could be made faster. This api is used to get responsetime of the host server, the time it takes untill the first content is rendered and the time untill the page is interactive.
#### BuiltWithApi
This api returns the technology stack which is beeing used on a website. An example would be a website uses drupal as a cms apache as a webserver etc. Calls to this api are highly ristricted with a free license. 
#### CustomApi
The last api that is being used is an api written and hosted by myself. This api retrieves information about how pages on a website are being connected and also if these connections are not broken (dont return 400-500 codes).
The endpoint is available at arne-baumann.de:9080/ and the project can be viewed here.

### Next Steps
The next step of the project would be to replace the highly restrictivy apis that are currently being used (e.g BuiltWithApi) and replace them with self developed apis.
