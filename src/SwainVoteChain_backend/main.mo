import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import List "mo:base/List";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import Bool "mo:base/Bool";
import Error "mo:base/Error";
import Hash "mo:base/Hash";
import Time "mo:base/Time";

actor SecureVoteChain {

  type Ballot = {
    id : Text; // Unique identifier for the ballot
    title : Text;
    description : Text;
    startDate : Nat; // Unix timestamp
    endDate : Nat; // Unix timestamp
    eligibleVoters : List<Principal>; // Eligible voters' IDs
    candidates : List<Candidate>; // List of candidates
    isActive : Bool; // Indicates if the ballot is active
  };

  var ballots = List.nil<Ballot>();

  type Voter = {
    id : Principal;
    name : Text;
    age : Nat;
    hasVoted : Bool;
  };

  type Candidate = {
    id : Principal;
    name : Text;
    party : Text;
    voteCount : Nat;
  };

  type Vote = {
    voter : Principal;
    candidate : Principal;
    time : Nat;
  };

  let voters = HashMap.HashMap<Principal, Voter>(0, Principal.equal, Principal.hash);
  var candidates = List.nil<Candidate>();
  let votes = List.nil<Vote>();

  type Result<A, B> = Result.Result<A, B>;
  type List<A> = ?(Principal, List<A>);

  // Create a new ballot
  public func createBallot(ballot : Ballot) : async Result<(), Text> {
    // Check if the ballot ID is unique
    switch (List.find(ballots, func(b : Ballot) : Bool { return b.id == ballot.id })) {
      case (?existingBallot) {
        return #err("Ballot ID already exists");
      };
      case (null) {
        // Add the new ballot
        ballots := List.push(ballot, ballots);
        return #ok(());
      };
    };
  };

  // Manage Ballot Status
  public func manageBallotStatus(ballotId : Text, action : Text) : async Result<(), Text> {
    // Find the specified ballot
    switch (List.find(ballots, func(b : Ballot) : Bool { return b.id == ballotId })) {
      case (?existingBallot) {
        // Check if the action is to start or end the ballot
        switch (action) {
          case ("start") {
            // Start the ballot
            if (existingBallot.isActive) {
              return #err("Ballot is already active");
            } else {
              // Check if the current time is after the start date
              let currentTime = Time.now();
              if (currentTime < existingBallot.startDate) {
                return #err("Ballot cannot be started before the start date");
              } else {
                // Update the ballot's isActive flag to indicate that it's active
                let updatedBallot = { existingBallot with isActive = true };
                let updatedBallots = List.map(
                  ballots,
                  func(b : Ballot) : Ballot {
                    if (b.id == ballotId) {
                      return updatedBallot;
                    } else {
                      return b;
                    };
                  },
                );
                ballots := updatedBallots;
                return #ok(());
              };
            };
          };
          case ("end") {
            // End the ballot
            if (not existingBallot.isActive) {
              return #err("Ballot is already inactive");
            } else {
              // Check if the current time is after the end date
              let currentTime = Time.now();
              if (currentTime < existingBallot.endDate) {
                return #err("Ballot cannot be ended before the end date");
              } else {
                // Update the ballot's isActive flag to indicate that it's inactive
                let updatedBallot = { existingBallot with isActive = false };
                let updatedBallots = List.map(
                  ballots,
                  func(b : Ballot) : Ballot {
                    if (b.id == ballotId) {
                      return updatedBallot;
                    } else {
                      return b;
                    };
                  },
                );
                ballots := updatedBallots;
                return #ok(());
              };
            };
          };
          case _ {
            return #err("Invalid action");
          };
        };
      };
      case (null) {
        return #err("Ballot not found");
      };
    };
  };

  // Update an existing ballot
  public func updateBallot(ballotId : Text, updatedBallot : Ballot) : async Result<(), Text> {
    switch (List.find(ballots, func(b : Ballot) : Bool { return b.id == ballotId })) {
      case (?existingBallot) {
        // Update the existing ballot properties
        let updatedBallots = List.map(
          ballots,
          func(b : Ballot) : Ballot {
            if (b.id == ballotId) {
              // Modify the properties you want to update
              return {
                existingBallot with
                title = updatedBallot.title;
                description = updatedBallot.description;
                startDate = updatedBallot.startDate;
                endDate = updatedBallot.endDate;
              };
            } else {
              return b;
            };
          },
        );
        ballots := updatedBallots;
        return #ok(());
      };
      case (null) {
        return #err("Ballot not found");
      };
    };
  };

  // Register candidate//
  public func registerCandidate(candidate : Candidate) : async Result<(), Text> {
    switch (List.find(candidates, func(c : Candidate) : Bool { return candidate.id == c.id })) {
      case (?candidate) {
        return #err("Already a member");
      };
      case (null) {

        candidates := List.push(candidate, candidates);
        return #ok(());
      };
    };
  };
  // Getting candidate with Principal
  public query func getCandidate(principal : Principal) : async ?Candidate {
    switch (List.find(candidates, func(c : Candidate) : Bool { return c.id == principal })) {
      case (?candidate) {
        return ?candidate;
      };
      case (null) {
        return null;
      };
    };
  };

  // get List of  all registerd Candidate
  public query func getAllCandidates() : async [Candidate] {
    return List.toArray(candidates);
  };

  // Register a new voter
  public shared ({ caller }) func registerVoter(voter : Voter) : async Result<(), Text> {
    switch (voters.get(caller)) {
      case (?voter) {
        return #err("Already a registered voter");
      };
      case (null) {
        if (voter.age < 18) {
          return #err("underage");
        };
        voters.put(caller, voter);
        return #ok(());
      };
    };
  };

  // Getting registerd voters with Principal
  public query func getVoters(vo : Principal) : async Result<(), Text> {
    switch (voters.get(vo)) {
      case (?voter) {
        return #ok(());
      };
      case (null) {
        return #err("Voter not found");
      };
    };
  };

  // get list of all registered voters
  public query func getAllVoter() : async [Voter] {
    return Iter.toArray(voters.vals());
  };

  // Cast a vote
  public shared ({ caller }) func castVote(vote : Vote) : async Result<(), Text> {
    switch (List.find(votes, func(v : Vote) : Bool { return v.voter == vote.voter })) {
      case (?existingVote) {
        return #err("You have already voted");
      };
      case (null) {
        switch (List.find(candidates, func(c : Candidate) : Bool { return c.id == vote.candidate })) {
          case (?candidate) {
            // Update the vote count for the candidate
            candidates := List.map(
              candidates,
              func(c : Candidate) : Candidate {
                if (c.id == vote.candidate) {
                  return { c with voteCount = c.voteCount + 1 };
                };
                c;
              },
            );

            // Add the vote to the list

            var vops = List.push(vote, votes);

            return #ok(());
          };
          case (null) {
            return #err("Candidate not found");
          };
        };
      };
    };
  };

  // Get vote counts
  public query func getVoteCounts() : async [Candidate] {
    return List.toArray(candidates);
  };

  // Announce the winner(s)
  public query func announceWinners() : async [Candidate] {
    // Find the candidate(s) with the highest vote count
    let maxVoteCount = List.foldLeft(
      candidates,
      0,
      func(max : Nat, c : Candidate) : Nat {
        if (c.voteCount > max) {
          c.voteCount;
        } else {
          max;
        };
      },
    );

    // Filter candidates with the highest vote count
    let winners = List.filter(
      candidates,
      func(c : Candidate) : Bool {
        return c.voteCount == maxVoteCount;
      },
    );

    return List.toArray(winners);
  };

};
