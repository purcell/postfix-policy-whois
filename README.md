Postfix Policy Daemon using whois data
======================================

This simple [postfix policy daemon](http://www.postfix.org/SMTPD_POLICY_README.html) rejects
emails sent from domains registered without publicly-accessible contact information, since
those domains are almost always owned by spammers.
