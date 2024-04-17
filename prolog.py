from pyswip.prolog import Prolog


prolog = Prolog()
prolog.consult('mystery_knowledge_base.pl')


def clear_and_assert(prolog, clues_list):
    # Clear existing facts from the knowledge base
    clear_queries = [
        "retractall(lab_access(_, _))",
        "retractall(background(_, _))",
        "retractall(evidence(_, _, _))"
    ]
    for query in clear_queries:
        list(prolog.query(query))  # Fully consume the generator to ensure facts are retracted

    # Assert new clues to Prolog and fully consume the generator
    for clue in clues_list:
        query = f"assertz({clue})"
        list(prolog.query(query))  # Ensure the query is fully consumed
        print(f"Asserted to Prolog: {clue}")
        
# Initial set of clues
initial_clues = [
    "lab_access(alex, false)",
    "background(alex, espionage)",
    "evidence(alex, threat, 'Received threatening emails')"
]
clear_and_assert(prolog, initial_clues)

# Check suspects and evidence requirements
print("Checking suspects and their evidence requirements:")
suspects = list(prolog.query("find_suspects(Suspect, Request)"))
for suspect in suspects:
    print(f"Suspect: {suspect['Suspect']}, Request: {suspect['Request']}")

# Call show_evidence and check_requirements
for answer in prolog.query("show_evidence(alex)"):
    print(answer)
for answer in prolog.query("check_requirements(alex)"):
    print(answer)

# Additional clues added
additional_clues = [
    "evidence(alex, bribe, 'Received money from a rival company')",
    "evidence(vostok_lab, footage, 'Footage showing unauthorized access')"
]
clear_and_assert(prolog, initial_clues + additional_clues)

# Attempt to deduce the mastermind
mastermind = list(prolog.query("deduce_mastermind(Suspect, Details)"))
if mastermind:
    print("Mastermind deduced:", mastermind)
else:
    print("Insufficient evidence to deduce the mastermind.")

# Call show_evidence and check_requirements again
for answer in prolog.query("show_evidence(alex)"):
    print(answer)
for answer in prolog.query("check_requirements(alex)"):
    print(answer)