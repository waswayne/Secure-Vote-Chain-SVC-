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


actor SecureVoteChain {

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

  // type List = ?(Principal, List);

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

                  var cast = c.voteCount + 1;
                  

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
