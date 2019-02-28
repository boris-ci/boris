
Customer Contexts:
 - A customer context is a collection of projects + billing + agents.
 - Use boris in only one context at a time.
 - individual:
      A user stumbles upon boris and logs in, they are in an individual context.
 - organisation:
      A user creates an organisation and switches to that organisation, they are in an organisational context.
 - global:
   - special case of organisation when boris is in single tenant mode




Repositories:
  - Exist in a customer context, can be shared between contexts.



Example:

thumphries/projector
  - markhibberd is contributor with read access

fork: markhibberd/projector
  - markhibberd is owner with adminaccess

Questions:

  - How does mark hibberd create a project for markhibberd/projector? Github Sync ; Project is listed ; Activate Project button ; Project: projector, Owner: markhibberd
  - How does mark hibberd refer trigger build for project markhibberd/projector? `boris build projector master`
  - How does mark hibberd create a project for thumphries/projector? He could do it manually, picks name himself, could add link to github, owner: marhibberd?
  - Is it different if he was an administrator? Github Sync; Project is listed ; Activate Project button ; Project: projector, Owner: thumphries, Shared with markhibberd? or forced to pick different name? owner markhibberd? Or prompted to choose between create owner/organisation or create personal build.
  - How does mark hibberd refer trigger build for project thumphries/projector? `boris build thumphries/projector master`
  - Tim' comes along, how does he buy into the project for thumphries/projector? if created as private, he could create an official one and share, if the organisation was getting created he would of been invited / asked for approval?
  - Is it the same as Mark's or a new build?
