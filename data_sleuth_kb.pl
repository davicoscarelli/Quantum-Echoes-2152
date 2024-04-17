:- dynamic person/3.
:- dynamic article/3.

% People profiles
person('Dr. Alice Smith', 'Tech Innovations Lab', ['AI', 'neural networks']).
person('Dr. Bob Jones', 'Quantum Computing Inc', ['quantum computing']).

% Articles
article('Quantum Computing Breakthrough', 'A major advancement in quantum computing has been achieved by Dr. Bob Jones.', ['quantum computing', 'breakthrough']).
article('AI Ethics Debate', 'The recent debate on AI ethics has sparked controversy across the tech world.', ['AI', 'ethics']).

% Search by tag
search(Tag, Result) :-
    person(Name, Affiliation, Tags),
    member(Tag, Tags),
    Result = person(Name, Affiliation, Tags);
    article(Title, Content, Tags),
    member(Tag, Tags),
    Result = article(Title, Content).
