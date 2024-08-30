# CAREFUL: 
# THESE SHOULD NOT CONTAIN BRACKETS IN THE HEADER OR THE IDENTIFIER
# EACH IDENTIFIER MUST BE TOTALLY DISTINCT
# NO FULL IDENTIFIER SHOULD BE A SUBSET OF ANY OTHER
# MAKE SURE THEY MATCH QUOTA FRAME LABELS

ind.features <- c(
  'ETHNICITY:
E1) white - individuals with origins in any of the original peoples of europe, including, for example, english, german, irish, italian, polish, and scottish -- as well as arab or middle-eastern with origins in any of the original peoples of the middle east or north africa, including, for example, lebanese, iranian, egyptian, syrian, iraqi, and israeli.
E2) black or african american - individuals with origins in any of the black racial groups of africa, including, for example, african american, jamaican, haitian, nigerian, ethiopian, and somali.
E3) hispanic or latino - includes individuals of mexican, puerto rican, salvadoran, cuban, dominican, guatemalan, and other central or south american or spanish culture or origin.
E4) asian - individuals with origins in any of the original peoples of central or east asia, southeast asia, or south asia, including, for example, chinese, asian indian, filipino, vietnamese, korean, and japanese.
E5) american indian or alaskan native or native hawaiian or pacific islander - individuals with origins in any of the original peoples of north, central, and south america, including, for example, navajo nation, blackfeet tribe of the blackfeet the indian reservation of montana, native village of barrow inupiat traditional government, nome eskimo community, aztec, and maya -- as well as individuals with origins in any of the original peoples of hawaii, guam, samoa, or other pacific islands, including, for example, native hawaiian, samoan, chamorro, tongan, fijian, and marshallese.
E6) multiracial - individuals who identify explicitly as belonging to more than one of the racial and ethnic groups above, such as biracial individuals with one white and one black parent, or those with a combination of asian and hispanic heritage, etc. mixed-race individuals often face unique social experiences, such as celebrating diverse cultural holidays, speaking multiple languages, and bridging different cultural perspectives within their families and communities.

',
  'AGE:
A1) under 18 years old
A2) 18 to 24 years old
A3) 25 to 34 years old
A4) 35 to 44 years old
A5) 45 to 54 years old
A6) 55 to 64 years old
A7) 65 or older

',
  'SEX:
S1) masculine sex - male
S2) feminine sex - female

',
  'INTEREST IN POLITICS:
I1) not interested at all in politics
I2) slightly interested in politics
I3) moderately interested in politics
I4) highly interested in politics

',
  'MARITAL STATUS:
M1) married - currently legally married and living with a spouse
M2) single - never married, including those who are legally separated
M3) divorced - legally divorced and not currently remarried
M4) widowed - spouse has passed away and not currently remarried

',
  "HIGHEST EDUCATIONAL QUALIFICATION:
Q1) completed education up to and including high school - high school diploma, vocational training, associate degree
Q2) completed education at the college or university level - bachelor's degree, master's degree, doctorate

",
  'HOUSEHOLD INCOME BRACKET:
H1) up to 25000 USD per year
H2) between 25000 and 50000 USD per year
H3) between 50000 and 75000 USD per year
H4) between 75000 and 100000 USD per year
H5) more than 100000 USD per year

',
  'GENERAL TRUST IN OTHER PEOPLE:
Tru1) always trust other people
Tru2) most of the time trust other people
Tru3) about half of the time trust other people
Tru4) some of the time trust other people
Tru5) never trust other people

',
  'PAYING ATTENTION TO THE 2024 PRESIDENTIAL ELECTION:
Att1) not paying attention at all to the 2024 Presidential election in the US
Att2) paying only a little attention to the 2024 Presidential election in the US
Att3) paying some attention to the 2024 Presidential election in the US
Att4) paying a lot of attention to the 2024 Presidential election in the US

',
  # https://www.icpsr.umich.edu/web/pages/instructors/setups/notes/party-identification.html
  'PARTISAN LOYALTIES:
Pid1) strongly identifies with Democrats 
Pid2) weakly identifies with Democrats
Pid3) independent closer to the Democrats
Pid4) independent not closer to either party 
Pid5) independent closer to the Republicans
Pid6) weakly identifies with Republicans
Pid7) strongly identifies with Republicans

',
  "IDEOLOGICALLY, THIS PERSON APPEARS TO BE:
Ide2) very ideologically liberal
Ide3) somewhat ideologically liberal 
Ide4) moderate in ideological orientation
Ide5) somewhat ideologically conservative
Ide6) very ideologically conservative

",
  'PAST VOTE - TURNOUT IN THE 2020 PRESIDENTIAL ELECTION:
Tpa1) no chance this individual turned out to vote - Probability: 0 - in the 2020 election for President in their state
Tpa2) highly unlikely this individual turned out to vote - Probability: 0.15 - in the 2020 election for President in their state
Tpa3) unlikely this individual turned out to vote - Probability: 0.3 - in the 2020 election for President in their state
Tpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2020 election for President in their state
Tpa5) likely this individual turned out to vote - Probability: 0.7 - in the 2020 election for President in their state
Tpa6) highly likely this individual turned out to vote - Probability: 0.85 - in the 2020 election for President in their state
Tpa7) certain this individual turned out to vote - Probability: 1 - in the 2020 election for President in their state

',
  'PAST VOTE - TURNOUT IN THE 2022 HOUSE OF REPRESENTATIVES ELECTION:
Thpa1) no chance this individual turned out to vote - Probability: 0 - in the 2022 elections for the House of Representatives in their congressional district
Thpa2) highly unlikely this individual turned out to vote - Probability: 0.15 - in the 2022 elections for the House of Representatives in their congressional district
Thpa3) unlikely this individual turned out to vote - Probability: 0.3 - in the 2022 elections for the House of Representatives in their congressional district
Thpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2022 elections for the House of Representatives in their congressional district
Thpa5) likely this individual turned out to vote - Probability: 0.7 - in the 2022 elections for the House of Representatives in their congressional district
Thpa6) highly likely this individual turned out to vote - Probability: 0.85 - in the 2022 elections for the House of Representatives in their congressional district
Thpa7) certain this individual turned out to vote - Probability: 1 - in the 2022 elections for the House of Representatives in their congressional district

',
#  'PAST VOTE - ENSURE PROBABILITIES IMPLIED BY CATEGORY SUM TO 1 ACROSS PAST VOTE IDENTIFIERS - DID NOT VOTE IN THE 2020 PRESIDENTIAL ELECTION:
#Tpa1) no chance this individual did not vote - Probability: 0 - in the 2020 election for President 
#Tpa2) highly unlikely this individual did not vote - Probability: 0.15 - in the 2020 election for President 
#Tpa3) unlikely this individual did not vote - Probability: 0.3 - in the 2020 election for President 
#Tpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2020 election for President
#Tpa5) likely this individual did not vote - Probability: 0.7 - in the 2020 election for President
#Tpa6) highly likely this individual did not vote - Probability: 0.85 - in the 2020 election for President
#Tpa7) certain this individual did not vote - Probability: 1 - in the 2020 election for President
#
#PAST VOTE - ENSURE PROBABILITIES IMPLIED BY CATEGORY SUM TO 1 ACROSS PAST VOTE IDENTIFIERS - VOTED FOR DONALD TRUMP, THE REPUBLICAN PARTY CANDIDATE, IN THE 2020 PRESIDENTIAL ELECTION:
#Rpa1) no chance this individual voted for Donald Trump - Probability: 0 - in the 2020 election for President
#Rpa2) highly unlikely this individual voted for Donald Trump - Probability: 0.15 - in the 2020 election for President
#Rpa3) unlikely this individual voted for Donald Trump - Probability: 0.3 - in the 2020 election for President
#Rpa4) 50-50 likelihood that this individual voted for Donald Trump - Probability: 0.5 - in the 2020 election for President
#Rpa5) likely this individual voted for Donald Trump - Probability: 0.7 - in the 2020 election for President
#Rpa6) highly likely this individual voted for Donald Trump - Probability: 0.85 - in the 2020 election for President
#Rpa7) certain this individual voted for Donald Trump - Probability: 1 - in the 2020 election for President
#
#PAST VOTE - ENSURE PROBABILITIES IMPLIED BY CATEGORY SUM TO 1 ACROSS PAST VOTE IDENTIFIERS - VOTED FOR JOE BIDEN, THE DEMOCRATIC PARTY CANDIDATE, IN THE 2020 PRESIDENTIAL ELECTION:
#Dpa1) no chance this individual voted for Joe Biden - Probability: 0 - in the 2020 election for President
#Dpa2) highly unlikely this individual voted for Joe Biden - Probability: 0.15 - in the 2020 election for President
#Dpa3) unlikely this individual voted for Joe Biden - Probability: 0.3 - in the 2020 election for President
#Dpa4) 50-50 likelihood that this individual voted for Joe Biden - Probability: 0.5 - in the 2020 election for President
#Dpa5) likely this individual voted for Joe Biden - Probability: 0.7 - in the 2020 election for President
#Dpa6) highly likely this individual voted for Joe Biden - Probability: 0.85 - in the 2020 election for President
#Dpa7) certain this individual voted for Joe Biden - Probability: 1 - in the 2020 election for President
#
#PAST VOTE - ENSURE PROBABILITIES IMPLIED BY CATEGORY SUM TO 1 ACROSS PAST VOTE IDENTIFIERS - VOTED FOR JO JORGENSEN, THE LIBERTARIAN PARTY CANDIDATE, IN THE 2020 PRESIDENTIAL ELECTION:
#Lpa1) no chance this individual voted for Jo Jorgensen - Probability: 0 - in the 2020 election for President
#Lpa2) highly unlikely this individual voted for Jo Jorgensen - Probability: 0.15 - in the 2020 election for President
#Lpa3) unlikely this individual voted for Jo Jorgensen - Probability: 0.3 - in the 2020 election for President
#Lpa4) 50-50 likelihood that this individual voted for Jo Jorgensen - Probability: 0.5 - in the 2020 election for President
#Lpa5) likely this individual voted for Jo Jorgensen - Probability: 0.7 - in the 2020 election for President
#Lpa6) highly likely this individual voted for Jo Jorgensen - Probability: 0.85 - in the 2020 election for President
#Lpa7) certain this individual voted for Jo Jorgensen - Probability: 1 - in the 2020 election for President
#
#PAST VOTE - ENSURE PROBABILITIES IMPLIED BY CATEGORY SUM TO 1 ACROSS PAST VOTE IDENTIFIERS - VOTED FOR HOWIE HAWKINS, THE GREEN PARTY CANDIDATE, IN THE 2020 PRESIDENTIAL ELECTION:
#Gpa1) no chance this individual voted for Howie Hawkins - Probability: 0 - in the 2020 election for President
#Gpa2) highly unlikely this individual voted for Howie Hawkins - Probability: 0.15 - in the 2020 election for President
#Gpa3) unlikely this individual voted for Howie Hawkins - Probability: 0.3 - in the 2020 election for President
#Gpa4) 50-50 likelihood that this individual voted for Howie Hawkins - Probability: 0.5 - in the 2020 election for President
#Gpa5) likely this individual voted for Howie Hawkins - Probability: 0.7 - in the 2020 election for President
#Gpa6) highly likely this individual voted for Howie Hawkins - Probability: 0.85 - in the 2020 election for President
#Gpa7) certain this individual voted for Howie Hawkins - Probability: 1 - in the 2020 election for President
#
#PAST VOTE - ENSURE PROBABILITIES IMPLIED BY CATEGORY SUM TO 1 ACROSS PAST VOTE IDENTIFIERS - VOTED FOR A CANDIDATE OTHER THAN THE REPUBLICAN, DEMOCRAT, LIBERTARIAN, OR GREEN PARTY CANDIDATES IN THE 2020 PRESIDENTIAL ELECTION:
#Opa1) no chance this individual voted for another candidate - Probability: 0 - in the 2020 election for President
#Opa2) highly unlikely this individual voted for another candidate - Probability: 0.15 - in the 2020 election for President
#Opa3) unlikely this individual voted for another candidate - Probability: 0.3 - in the 2020 election for President
#Opa4) 50-50 likelihood that this individual voted for another candidate - Probability: 0.5 - in the 2020 election for President
#Opa5) likely this individual voted for another candidate - Probability: 0.7 - in the 2020 election for President
#Opa6) highly likely this individual voted for another candidate - Probability: 0.85 - in the 2020 election for President
#Opa7) certain this individual voted for another candidate - Probability: 1 - in the 2020 election for President
#
#',
  'PAST VOTE - VOTE CHOICE IN THE 2020 PRESIDENTIAL ELECTION:
Vpa1) did not vote in the 2020 election for President in their state
Vpa2) voted for Donald Trump, the Republican Party candidate, in the 2020 election for President in their state
Vpa3) voted for Joe Biden, the Democratic Party candidate, in the 2020 election for President in their state
Vpa4) voted for Jo Jorgensen, the Libertarian Party candidate, in the 2020 election for President in their state
Vpa5) voted for Howie Hawkins, the Green Party candidate, in the 2020 election for President in their state
Vpa6) voted for a candidate other than the Republican, Democrat, Libertarian, or Green Party candidates, in the 2020 election for President in their state

',
  'PAST VOTE - VOTE CHOICE IN THE 2022 HOUSE OF REPRESENTATIVES ELECTION:
Vhpa1) did not vote in the 2022 elections for the House of Representatives in their congressional district
Vhpa2) voted for a Republican Party candidate in the 2022 elections for the House of Representatives in their congressional district
Vhpa3) voted for a Democratic Party candidate in the 2022 elections for the House of Representatives in their congressional district
Vhpa4) voted for a Libertarian Party candidate, in the 2022 elections for the House of Representatives in their congressional district
Vhpa5) voted for a Green Party candidate, in the 2022 elections for the House of Representatives in their congressional district
Vhpa6) voted for a candidate other than the Republican, Democrat, Libertarian, or Green Party candidates, in the 2022 elections for the House of Representatives in their congressional district

'
)

geo2_prompt <- 'Which state of the USA do they live in?
If they do not specify a state, but are still from the United States of America, write "USA".
If they are not from a state in the USA, write "Not from a state in the USA".
Write out just the full name of the state.
If they are from the District of Columbia, also known as Washington D.C., write "District of Columbia".'

geo3_prompt <- "Considering all the information at your disposal, write out the most likely Congressional District in the state to which the individual belongs.
Write out only the full name of the district, e.g., 'Virginia 1st'; 'Florida 2nd'; 'Alaska at Large', etc.
For the selected district, please note the level of Speculation, on a scale from 0 (not speculative at all, every single element of the user data is directly informative of the exact district) to 100 (fully speculative, there is no information which is helpful in determining the exact district in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

Explain at length what features of the data contributed to your choice and your speculation level.
Preserve a strictly structured answer to ease parsing of the text."

geo3_parse_prompt <- "Extract the US Congressional District which the text implies is the most likely for an individual to live in, and the speculation level with which this choice was made.
Return the district in the format implied by these examples: 'District: Virginia 1st - Speculation: 90'; 'District: Florida 2nd - Speculation: 10'; 'District: Alaska at Large - Speculation: 50'; etc.
Do not return any other unnecessary writing.
If they are from the District of Columbia, also known as Washington D.C., write 'District of Columbia at Large'.
If you cannot determine a US Congressional District from the information provided, answer 'Don't Know', and assign a Speculation score of 100."

geo3_pastvote_congress_candidatesprompt <- "Based on what you know of the candidates in the 2022 congressional district election held in this district on November 8, 2022, please complete the following set of questions and their options.
If there are no candidates for the given party, remove the option related to the given party entirely -- do not present that party's option at all.
If there is more than one candidate for a single party, write out each option in two separate lines, and assign a different symbol for the identifier to each.
If there is only one candidate in the background information, only inlcude that candidate as an option, along with `did not vote`.
Carefully ensure symbols associated with specific options are not repeated -- for instance, if you have a `Tpa1` in your answer, ensure every other `Tpa` related symbol is different - e.g. Tpa2, Tpa3, etc. 

Below is the set of questions and options for you to complete - your job is to replace the instances wrapped in <...> with the correct knowledge for this congressional district.
Do not produce any other text beyond the completed set of questions.


PAST VOTE - TURNOUT IN THE 2022 HOUSE OF REPRESENTATIVES ELECTION:
Tpa1) no chance this individual turned out to vote - Probability: 0 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Tpa2) highly unlikely this individual turned out to vote - Probability: 0.15 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Tpa3) unlikely this individual turned out to vote - Probability: 0.3 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Tpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Tpa5) likely this individual turned out to vote - Probability: 0.7 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Tpa6) highly likely this individual turned out to vote - Probability: 0.85 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Tpa7) certain this individual turned out to vote - Probability: 1 - in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>

PAST VOTE - VOTE CHOICE IN THE 2022 HOUSE OF REPRESENTATIVES ELECTION:
Vpa1) did not vote in the 2022 congressional district election in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_REPUBLICAN_CANDIDATE_NAME_HERE>, the Republican Party candidate, in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_DEMOCRATIC_CANDIDATE_NAME_HERE>, the Democratic Party candidate, in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_LIBERTARIAN_CANDIDATE_NAME_HERE>, the Libertarian Party candidate, in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_GREEN_CANDIDATE_NAME_HERE>, the Green Party candidate, in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_INDEPENDENT_CANDIDATE_NAME_HERE>, an Independent candidate, in the 2022 elections for the House of Representatives in <INSERT_CONGRESSIONAL_DISTRICT_NAME_HERE>"

geo3_pastvote_congress_candidatesprompt_default <- "PAST VOTE - TURNOUT IN THE 2022 HOUSE OF REPRESENTATIVES ELECTION:
Tpa1) no chance this individual turned out to vote - Probability: 0 - in the 2022 elections for the House of Representatives in their congressional district
Tpa2) highly unlikely this individual turned out to vote - Probability: 0.15 - in the 2022 elections for the House of Representatives in their congressional district
Tpa3) unlikely this individual turned out to vote - Probability: 0.3 - in the 2022 elections for the House of Representatives in their congressional district
Tpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2022 elections for the House of Representatives in their congressional district
Tpa5) likely this individual turned out to vote - Probability: 0.7 - in the 2022 elections for the House of Representatives in their congressional district
Tpa6) highly likely this individual turned out to vote - Probability: 0.85 - in the 2022 elections for the House of Representatives in their congressional district
Tpa7) certain this individual turned out to vote - Probability: 1 - in the 2022 elections for the House of Representatives in their congressional district

PAST VOTE - VOTE CHOICE IN THE 2022 HOUSE OF REPRESENTATIVES ELECTION:
Vpa1) did not vote in the 2022 elections for the House of Representatives in their congressional district
Vpa2) voted for the Republican Party candidate in the 2022 elections for the House of Representatives in their congressional district
Vpa3) voted for the Democratic Party candidate in the 2022 elections for the House of Representatives in their congressional district
Vpa4) voted for the Libertarian Party candidate in the 2022 elections for the House of Representatives in their congressional district
Vpa5) voted for the Green Party candidate in the 2022 elections for the House of Representatives in their congressional district
Vpa6) voted for a candidate other than the Republican, Democrat, Libertarian, or Green Party candidates, in the 2022 elections for the House of Representatives in their congressional district"



geo2_pastvote_president_candidatesprompt <- "Based on what you know of the candidates in the 2020 Presidential election held in this state on November 3, 2020, please complete the following set of questions and their options.
If there are no candidates for the given party, remove the option related to the given party entirely -- do not present that party's option at all.
If there is more than one candidate for a single party, write out each option in two separate lines, and assign a different symbol for the identifier to each.
Below is the set of questions and options for you to complete - your job is to replace the instances wrapped in <...> with the correct knowledge for this state.
Do not produce any other text beyond the completed set of questions.

PAST VOTE - TURNOUT IN THE 2020 PRESIDENTIAL ELECTION:
Tpa1) no chance this individual turned out to vote - Probability: 0 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Tpa2) highly unlikely this individual turned out to vote - Probability: 0.15 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Tpa3) unlikely this individual turned out to vote - Probability: 0.3 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Tpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Tpa5) likely this individual turned out to vote - Probability: 0.7 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Tpa6) highly likely this individual turned out to vote - Probability: 0.85 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Tpa7) certain this individual turned out to vote - Probability: 1 - in the 2020 election for President in <INSERT_STATE_NAME_HERE>

PAST VOTE - VOTE CHOICE IN THE 2020 PRESIDENTIAL ELECTION:
Vpa1) did not vote in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_REPUBLICAN_CANDIDATE_NAME_HERE>, the Republican Party candidate, in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_DEMOCRATIC_CANDIDATE_NAME_HERE>, the Democratic Party candidate, in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_LIBERTARIAN_CANDIDATE_NAME_HERE>, the Libertarian Party candidate, in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_GREEN_CANDIDATE_NAME_HERE>, the Green Party candidate, in the 2020 election for President in <INSERT_STATE_NAME_HERE>
Vpa<INSERT_OPTION_NUMBER_HERE>) voted for <INSERT_INDEPENDENT_CANDIDATE_NAME_HERE>, an Independent candidate, in the 2020 election for President in <INSERT_STATE_NAME_HERE>"

geo2_pastvote_president_candidatesprompt_default <- "PAST VOTE - TURNOUT IN THE 2020 PRESIDENTIAL ELECTION:
Tpa1) no chance this individual turned out to vote - Probability: 0 - in the 2020 elections for President in their state
Tpa2) highly unlikely this individual turned out to vote - Probability: 0.15 - in the 2020 elections for President in their state
Tpa3) unlikely this individual turned out to vote - Probability: 0.3 - in the 2020 elections for President in their state
Tpa4) 50-50 likelihood that this individual voted - Probability: 0.5 - in the 2020 elections for President in their state
Tpa5) likely this individual turned out to vote - Probability: 0.7 - in the 2020 elections for President in their state
Tpa6) highly likely this individual turned out to vote - Probability: 0.85 - in the 2020 elections for President in their state
Tpa7) certain this individual turned out to vote - Probability: 1 - in the 2020 elections for President in their state

PAST VOTE - VOTE CHOICE IN THE 2020 PRESIDENTIAL ELECTION:
Vpa1) did not vote in the 2020 elections for President in their state
Vpa2) voted for Donald Trump, the Republican Party candidate in the 2020 elections for President in their state
Vpa3) voted for Joe Biden, the Democratic Party candidate in the 2020 elections for President in their state
Vpa4) voted for Jo Jorgensen, the Libertarian Party candidate in the 2020 elections for President in their state
Vpa5) voted for Howie Hawkins, the Green Party candidate in the 2020 elections for President in their state
Vpa6) voted for a candidate other than the Republican, Democrat, Libertarian, or Green Party candidates, in the 2020 elections for President in their state"
