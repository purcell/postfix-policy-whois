Postfix Policy Daemon using whois data
======================================

This simple [postfix policy daemon](http://www.postfix.org/SMTPD_POLICY_README.html) rejects
emails sent from domains registered without publicly-accessible contact information, since
those domains are often owned by spammers. Some mail server administrators might consider this
too draconian for their tastes.

The policy daemon also rejects mail from domains which are pending expiry, or are in their initial
grace period after new registration ("ADDPERIOD"): sophisticated spam outfits frequently
register fresh domains and provision servers which send mail that passes all reasonable header checks

Rejecting mail during the sending domain's ADDPERIOD gives time for others to report abuse to the
hosting provider, and allows spamcop and other blocklist maintainers a chance to add that domain.

*Current status: testing privately in a moderately large production environment*

<hr>

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)

