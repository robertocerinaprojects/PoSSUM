THINGS TO SET:
- set independent and dependent variables 
- set geo candidates lookup and context 
- set ballot access lookup -- CHECK THIS BEFORE RUNNING NEW POLL, IDEALLY RUN THIS ANEW AND CHECK THAT IT MAKES SENSE
- set quota frame (including target N for this poll) -- CHECK THIS BEFORE RUNNING NEW POLL
- set get_pool parameters (including X API query type and number of desired tweets) -- CHECK THIS BEFORE RUNNING NEW POLL
- open environment file for poll_users and set m (number of tweets to extract) and tau (temporal inclusion criteria)  -- CHECK THIS BEFORE RUNNING NEW POLL
- open exe file for overall polling and set POOL_TYPE and START_NEW_QUOTA_COUNTER flag


#######AUGUST 15th, starting a new poll at 15:30
- Re-Ran ballot access script and checked 
- POOL_TYPE = latest
- START_NEW_QUOTA_COUNTER = TRUE
- n_trends = 40*50
- n_political = 2000
- Omega^star = 1500
- tau = 7
- m = 20