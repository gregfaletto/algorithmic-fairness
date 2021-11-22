# algorithmic-fairness
These are some resources I've developed on the topic of algorithmic fairness.

* **Algorithmic Fairness--An Overview:** These are slides for an in-class presentation I did on algorithmic fairness. They're a good place to start.
* **Algorithmic Fairness paper:** This is a paper I wrote discussing the basics of algorithmic fairness and some common fairness criteria. This paper covers the topics from the slides in more detail.
* **Fairness Criteria Examples:** These are some slides that illustrate how some common fairness criteria work in an example in the setting of college admissions. Given college applicants from two groups, I show how different possible sets of students to admit would satisfy or violate three commonly proposed standards of algorithmic fairness: *demographic parity*, *equalized odds*, and *calibration*.
    * In more detail, this is the setting: a university receives applications from 12 students from Group A and 16 students from Group B.  In something like the [potential outcomes framework](https://en.wikipedia.org/wiki/Rubin_causal_model), each applicant has a label of either "qualified" or "unqualified," based on what would happen if the student were admitted to the university.
        * For example, we could say that all applicants who would graduated within 5 years in the event that they were admitted would be "qualified," and all students who would not graduated within 5 years are "unqualified.")
        * At the time that the university has to decide who to admit, they of course do not directly observe which students are "qualified" and "unqualified;" the university attempts to estimate who is qualified using the students' applications. This is one reason why the university may end up admitting "unqualified" students. However, for equalized odds and calibration, whether or not a given assortment of students who are admitted is considered "fair" will depend on how many qualified and unqualified students are admitted from each group.
    * In these slides, I walk through several examples of possible assortments of students the university could choose to admit. For each example, I explain which of the three fairness criteria are met and which are not, and why. 
        * Readers will likely find that more than one of the fairness criteria sounds reasonable to them (if not all three), so it may be surprising to see how tricky it is to satisfy more than one of them at a time. 
        * Further, some assortments that are in some sense "fair" are obviously undesirable in the sense that they result in admitting more unqualified applicants than qualified applicants, and so on.
        * Another interesting observation is that in some cases two fairness criteria agree that admitting an assortment of students is unfair, but they disagree on for whom--by one criterion the assortment is unfair to Group A, and by another the assortment is unfair to Group B.
    * These examples provide an intuition as to what each criterion enforces, the tradeoffs between different criteria, and why they can almost never all be satisfied. 
    * I created these slides for an invited talk that [Prof. Kenneth Silver](https://www.kennethmsilver.com/) (Assistant Professor in Business Ethics at Trinity College Dublin) and I gave at the [Workshop on Algorithmic Fairness](https://ps.au.dk/en/cepdisc/events/event/artikel/workshop-on-algorithmic-fairness/) hosted by the University of Copenhagen in November 2020. (The absract from our talk is [here](https://ps.au.dk/fileadmin/Statskundskab/CEPDISC/Abstracts_Copenhagen_Workshop_on_Algorithmic_Fairness_Nov_2020.pdf).)
