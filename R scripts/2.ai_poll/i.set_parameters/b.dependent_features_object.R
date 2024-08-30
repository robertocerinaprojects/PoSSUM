# CAREFUL: 
# THESE SHOULD NOT COINTAIN BRACKETS IN THE HEADER OR THE IDENTIFIER
# EACH IDENTIFIER MUST BE TOTALLY DISTINCT
# NO FULL IDENTIFIER SHOULD BE A SUBSET OF ANY OTHER

dep.features <- c(
#  "2024 PRESIDENTIAL ELECTION LIKELIHOOD OF TURNOUT - IF THE ELECTION WERE HELD ON THE DATE OF THEIR MOST RECENT TWEET:
#Tur1) no chance this individual turns out to vote - Probability: 0 - in the 2024 election for President 
#Tur2) highly unlikely this individual turns out to vote - Probability: 0.15 - in the 2024 election for President 
#Tur3) unlikely this individual turns out to vote - Probability: 0.3 - in the 2024 election for President 
#Tur4) 50-50 likelihood that this individual votes - Probability: 0.5 - in the 2024 election for President
#Tur5) likely this individual turns out to vote - Probability: 0.7 - in the 2024 election for President
#Tur6) highly likely this individual turns out to vote - Probability: 0.85 - in the 2024 election for President
#Tur7) certain this individual turns out to vote - Probability: 1 - in the 2024 election for President
#
#",
#  "2024 US PRESIDENTIAL ELECTION VOTE CHOICE - IF THE ELECTION WERE HELD ON THE DATE OF THEIR MOST RECENT TWEET:
#Vot1) would not vote
#Vot2) voting for Donald Trump, the Republican Party candidate
#Vot3) voting for Kamala Harris, the Democratic Party candidate
#Vot4) voting for Robert F. Kennedy Jr., who is not affiliated with any political party
#Vot5) voting for Dr. Cornel West, who is not affiliated with any political party
#Vot6) voting for Chase Oliver, the Libertarian Party candidate
#Vot7) voting for Jill Stein, the Green Party candidate
#
#",
  "UNDECIDEDNESS AROUND 2024 US PRESIDENTIAL ELECTION VOTE CHOICE - HOW LIKELY IS THE USER TO CHANGE THEIR MIND ABOUT THEIR VOTING BEHAVIOUR FOR THE 2024 US PRESIDENTIAL ELECTION, BETWEEN NOW -- DATE OF THEIR LATEST TWEET -- AND ELECTION DAY ON NOVEMBER 5th 2024:
Und1) no chance this individual will change their mind - Probability: 0 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024
Und2) highly unlikely this individual will change their mind - Probability: 0.15 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024
Und3) unlikely this individual will change their mind - Probability: 0.3 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024
Und4) 50-50 likelihood that this individual will change their mind - Probability: 0.5 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024
Und5) likely this individual will change their mind - Probability: 0.7 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024
Und6) highly likely this individual will change their mind - Probability: 0.85 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024
Und7) certain this individual will change their mind - Probability: 1 - as to which Presidential candidate to cast a ballot for - or whether they will stay home and not cast a ballot at all - on election day November 5th 2024

",
  "JOE BIDEN PRESIDENTIAL JOB APPROVAL:
Bap1) Strongly Approves of the way Joe Biden is handling his job as president
Bap2) Somewhat Approves of the way Joe Biden is handling his job as president
Bap3) Somewhat Disapproves of the way Joe Biden is handling his job as president
Bap4) Strongly Disapproves of the way Joe Biden is handling his job as president

",
  "FAVOURABILITY OF US POLITICIAN JOE BIDEN:
Bfa1) Very favourable view of Joe Biden
Bfa2) Somewhat favourable view of Joe Biden
Bfa3) Somewhat unfavourable view of Joe Biden
Bfa4) Very unfavourable view of Joe Biden
Bfa5) This person is unlikely to know who Joe Biden is, so they cannot have an opinion about them

",  
  "FAVOURABILITY OF US POLITICIAN KAMALA HARRIS:
Kfa1) Very favourable view of Kamala Harris
Kfa2) Somewhat favourable view of Kamala Harris
Kfa3) Somewhat unfavourable view of Kamala Harris
Kfa4) Very unfavourable view of Kamala Harris
Kfa5) This person is unlikely to know who Kamala Harris is, so they cannot have an opinion about them

",
  "FAVOURABILITY OF US POLITICIAN TIM WALZ:
Twa1) Very favourable view of Tim Walz
Twa2) Somewhat favourable view of Tim Walz
Twa3) Somewhat unfavourable view of Tim Walz
Twa4) Very unfavourable view of Tim Walz
Twa5) This person is unlikely to know who Tim Walz is, so they cannot have an opinion about them

",
  "FAVOURABILITY OF US POLITICIAN DONALD TRUMP:
Tfa1) Very favourable view of Donald Trump
Tfa2) Somewhat favourable view of Donald Trump
Tfa3) Somewhat unfavourable view of Donald Trump
Tfa4) Very unfavourable view of Donald Trump
Tfa5) This person is unlikely to know who Donald Trump is, so they cannot have an opinion about them

",
  "FAVOURABILITY OF US POLITICIAN JD VANCE:
Jfa1) Very favourable view of JD Vance
Jfa2) Somewhat favourable view of JD Vance
Jfa3) Somewhat unfavourable view of JD Vance
Jfa4) Very unfavourable view of JD Vance
Jfa5) This person is unlikely to know who JD Vance is, so they cannot have an opinion about them

",
  'FAVOURABILITY OF US POLITICIAN ROBERT F. KENNEDY JR.:
Rfa1) Very favourable view of Robert F. Kennedy Jr.
Rfa2) Somewhat favourable view of Robert F. Kennedy Jr.
Rfa3) Somewhat unfavourable view of Robert F. Kennedy Jr.
Rfa4) Very unfavourable view of Robert F. Kennedy Jr.
Rfa5) This person is unlikely to know who Robert F. Kennedy Jr. is, so they cannot have an opinion about them

',
  'FAVOURABILITY OF US POLITICIAN CORNEL WEST:
Wfa1) Very favourable view of Cornel West
Wfa2) Somewhat favourable view of Cornel West
Wfa3) Somewhat unfavourable view of Cornel West
Wfa4) Very unfavourable view of Cornel West
Wfa5) This person is unlikely to know who Cornel West is, so they cannot have an opinion about them

',
  'FAVOURABILITY OF US POLITICIAN JILL STEIN:
Sfa1) Very favourable view of Jill Stein
Sfa2) Somewhat favourable view of Jill Stein
Sfa3) Somewhat unfavourable view of Jill Stein
Sfa4) Very unfavourable view of Jill Stein
Sfa5) This person is unlikely to know who Jill Stein is, so they cannot have an opinion about them

',
  'FAVOURABILITY OF US POLITICIAN CHASE OLIVER:
Ofa1) Very favourable view of Chase Oliver
Ofa2) Somewhat favourable view of Chase Oliver
Ofa3) Somewhat unfavourable view of Chase Oliver
Ofa4) Very unfavourable view of Chase Oliver
Ofa5) This person is unlikely to know who Chase Oliver is, so they cannot have an opinion about them

',
  'BELIEF IN MOST IMPORTANT ISSUE TODAY:
Moi01) jobs and the economy are the most important issue for this user today
Moi02) immigration is the most important issue for this user today
Moi03) climate change and the environment are the most important issue for this user today
Moi04) foreign policy is the most important issue for this user today
Moi05) national security is the most important issue for this user today
Moi06) education is the most important issue for this user today
Moi07) healthcare is the most important issue for this user today
Moi08) taxes and government spending are the most important issue for this user today
Moi09) abortion is the most important issue for this user today
Moi10) civil rights and racism are the most important issue for this user today
Moi11) guns are the most important issue for this user today
Moi12) crime is the most important issue for this user today
Moi13) criminal justice reform and over-incarceration are the most important issue for this user today
Moi14) inflation and the cost of living is the most important issue for this user today

',
  'HAPPINESS LEVEL:
Hap1) very happy - consistently feels a high level of joy and satisfaction with their life - they often experience positive emotions, have an optimistic outlook, and find great contentment in their daily activities.
Hap2) rather happy - generally feels happy and satisfied with their life - they experience positive emotions frequently, though not as intensely as someone who is very happy, and have a positive outlook on life.
Hap3) not very happy - experiences some level of dissatisfaction or lack of joy in their life - they may feel positive emotions occasionally, but they often feel neutral or mildly negative about their overall life satisfaction.
Hap4) not happy at all - consistently feels unhappy and dissatisfied with their life - they frequently experience negative emotions, have a pessimistic outlook, and struggle to find joy in their daily activities.

',
  
  # for some of these personality traits, the middling option is being used as a
  # `Unsure` category
  'BIG 5 PERSONALITY TRAIT -- OPENNESS TO EXPERIENCE:
Ope1) very open to experience - highly curious, imaginative, and open to exploring new ideas and experiences - they are often creative and willing to take risks.
Ope2) somewhat open to experience - open to new experiences and ideas, but they may prefer some level of routine and familiarity - they are flexible but may not seek out novelty actively.
Ope3) neither open nor closed to experience - shows a balanced approach, being neither particularly eager to seek out new experiences nor strictly avoiding them - they may try new things occasionally but generally stick to known preferences.
Ope4) somewhat closed to experience - tends to prefer familiar routines and may be resistant to change - they are less likely to seek out new experiences or ideas but can adapt when necessary.
Ope5) very closed to experience - prefers a highly structured and predictable environment - they are resistant to change and new ideas, often sticking to what they know and avoiding novel experiences.

',
  'BIG 5 PERSONALITY TRAIT -- CONSCIENTIOUSNESS:
Con1) very conscientious - highly organized, responsible, and dependable - they pay great attention to detail and are very disciplined in their actions, often setting and achieving goals with precision.
Con2) somewhat conscientious - generally reliable and organized but may occasionally lapse in discipline or attention to detail - they can balance between structured and flexible approaches.
Con3) neither conscientious nor unconscientious - demonstrates an average level of conscientiousness, showing a balance between being organized and flexible - they are reliable but not overly meticulous or careless.
Con4) somewhat unconscientious - tends to be less organized and may struggle with maintaining discipline or consistency - they might prioritize spontaneity over structure.
Con5) very unconscientious - often lacks organization and discipline - they may be perceived as careless or unreliable, preferring a spontaneous and unstructured approach to tasks and responsibilities.

',
  'BIG 5 PERSONALITY TRAIT -- EXTRAVERSION:
Ext1) very extraverted - highly outgoing, energetic, and enjoys social interactions - they thrive in social settings, seek out company, and are often perceived as enthusiastic and talkative.
Ext2) somewhat extraverted - enjoys social interactions and can be outgoing, but they also appreciate some alone time - they are comfortable in social settings but do not seek them out as intensely as very extraverted individuals.
Ext3) neither extraverted nor introverted - displays a balanced mix of extraverted and introverted traits - they are equally comfortable being social or spending time alone, without a strong preference for either.
Ext4) somewhat introverted - tends to be more reserved and prefers quieter, less stimulating environments - they can engage in social activities but generally prefer smaller groups or solo activities.
Ext5) very introverted - highly reserved, enjoys solitude, and prefers minimal social interaction - they often feel drained by social settings and seek out peaceful, solitary environments.

',
  "BIG 5 PERSONALITY TRAIT -- AGREEABLENESS:
Agr1) very agreeable - highly cooperative, compassionate, and trusting - they prioritize harmony and are very considerate of others' feelings and needs, often putting others before themselves.
Agr2) somewhat agreeable - generally kind and cooperative but may occasionally assert their own needs - they balance between being considerate and standing up for themselves.
Agr3) neither agreeable nor disagreeable - generally kind and cooperative but may occasionally assert their own needs - they balance between being considerate and standing up for themselves.
Agr4) somewhat disagreeable - tends to be more competitive and less concerned with others' feelings - they may prioritize their own needs and can be perceived as more assertive or critical.
Agr5) very disagreeable - often uncooperative and less empathetic - they prioritize their own interests and are typically skeptical of others' intentions, often coming across as tough-minded and critical.

",
  'BIG 5 PERSONALITY TRAIT -- NEUROTICISM:
Neu1) very high in neuroticism - frequently experiences intense emotions such as anxiety, worry, and sadness - they may be more prone to stress and emotional instability, often reacting strongly to challenges and setbacks.
Neu2) somewhat high in neuroticism - tends to experience negative emotions more often than average but not as intensely - they may feel anxious or stressed at times but can also manage these feelings effectively in many situations.
Neu3) neither high nor low in neuroticism - has an average level of emotional stability - they experience a balanced range of emotions and are generally able to cope with stress and negative feelings without being overwhelmed.
Neu4) somewhat low in neuroticism - generally calm and emotionally stable - they experience negative emotions less frequently and can handle stress and challenges with relative ease.
Neu5) very low in neuroticism - exceptionally calm, resilient, and emotionally stable - they rarely feel anxious or stressed and can maintain a positive outlook even in difficult situations.

'
)



geo2_currentvote_president_candidatesprompt <- "Based on what you know of the candidates in the 2024 Presidential election held in this state on November 5, 2024, please complete the following set of questions and their options.
If there are no candidates for the given party, remove the option related to the given party entirely -- do not present that party's option at all.
If there is more than one candidate for a single party, write out each option in two separate lines, and assign a different symbol for the identifier to each.
Below is the set of questions and options for you to complete - your job is to replace the instances wrapped in <...> with the correct knowledge for this state.
Do not produce any other text beyond the completed set of questions.

CURRENT VOTING PREFERENCES - TURNOUT IN THE 2024 PRESIDENTIAL ELECTION IF THE ELECTION WERE HELD ON THE DATE OF THEIR MOST RECENT TWEET:
Tcu1) no chance this individual will turn-out to vote - Probability: 0 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Tcu2) highly unlikely this individual will turn-out to vote - Probability: 0.15 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Tcu3) unlikely this individual will turn-out to vote - Probability: 0.3 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Tcu4) 50-50 likelihood that this individual will vote - Probability: 0.5 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Tcu5) likely this individual will turn-out to vote - Probability: 0.7 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Tcu6) highly likely this individual will turn-out to vote - Probability: 0.85 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Tcu7) certain this individual will turn-out to vote - Probability: 1 - in the 2024 election for President in <INSERT_STATE_NAME_HERE>

CURRENT VOTING PREFERENCES - VOTE CHOICE IN THE 2024 PRESIDENTIAL ELECTION IF THE ELECTION WERE HELD ON THE DATE OF THEIR MOST RECENT TWEET:
Vcu1) would not vote in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Vcu<INSERT_OPTION_NUMBER_HERE>) would vote for <INSERT_REPUBLICAN_CANDIDATE_NAME_HERE>, the Republican Party candidate, in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Vcu<INSERT_OPTION_NUMBER_HERE>) would vote for <INSERT_DEMOCRATIC_CANDIDATE_NAME_HERE>, the Democratic Party candidate, in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Vcu<INSERT_OPTION_NUMBER_HERE>) would vote for <INSERT_LIBERTARIAN_CANDIDATE_NAME_HERE>, the Libertarian Party candidate, in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Vcu<INSERT_OPTION_NUMBER_HERE>) would vote for <INSERT_GREEN_CANDIDATE_NAME_HERE>, the Green Party candidate, in the 2024 election for President in <INSERT_STATE_NAME_HERE>
Vcu<INSERT_OPTION_NUMBER_HERE>) would vote for <INSERT_INDEPENDENT_CANDIDATE_NAME_HERE>, a candidate who is not affiliated with any political party, in the 2024 election for President in <INSERT_STATE_NAME_HERE>"

geo2_currentvote_president_candidatesprompt_default <- "CURRENT VOTING PREFERENCES - TURNOUT IN THE 2024 PRESIDENTIAL ELECTION IF THE ELECTION WERE HELD ON THE DATE OF THEIR MOST RECENT TWEET:
Tcu1) no chance this individual would turn-out to vote - Probability: 0 - in the 2024 elections for President in their state
Tcu2) highly unlikely this individual would turn-out to vote - Probability: 0.15 - in the 2024 elections for President in their state
Tcu3) unlikely this individual would turn-out to vote - Probability: 0.3 - in the 2024 elections for President in their state
Tcu4) 50-50 likelihood that this individual would vote - Probability: 0.5 - in the 2024 elections for President in their state
Tcu5) likely this individual would turn-out to vote - Probability: 0.7 - in the 2024 elections for President in their state
Tcu6) highly likely this individual would turn-out to vote - Probability: 0.85 - in the 2024 elections for President in their state
Tcu7) certain this individual would turn-out to vote - Probability: 1 - in the 2024 elections for President in their state

CURRENT VOTING PREFERENCES - VOTE CHOICE IN THE 2024 PRESIDENTIAL ELECTION IF THE ELECTION WERE HELD ON THE DATE OF THEIR MOST RECENT TWEET:
Vcu1) would not vote in the 2024 elections for President in their state
Vcu2) would vote for Donald Trump, the Republican Party candidate in the 2024 elections for President in their state
Vcu3) would vote for Kamala Harris, the Democratic Party candidate in the 2024 elections for President in their state
Vcu4) would vote for Robert F. Kennedy Jr., who is not affiliated with any political party, in the 2024 elections for President in their state
Vcu5) would vote for Jill Stein, the Green Party candidate in the 2024 elections for President in their state
Vcu6) would vote for Chase Oliver, the Libertarian Party candidate in the 2024 elections for President in their state
Vcu7) would vote for Dr. Cornel West, who is not affiliated with any political party, in the 2024 elections for President in their state"



