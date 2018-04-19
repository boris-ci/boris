```
+---------------------------------------------+                                                                                                                        
|       Single Tenant No Authentication       |                                                                                                                        
+---------------------------------------------+                                                                                                                        
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                       +--------------------+                                                                                                          
         .-.                           |All Projects:       |                                                                                                          
        (   )------------------------> |Read/Write/Build    |                                                                                                          
         `-'                           |                    |                                                                                                          
                                       +--------------------+                                                                                                          
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
 +---------------------------------------------+                                                                                                                       
 |     Single Tenant GitHub Authentication     |                                                                                                                       
 +---------------------------------------------+                                                                                                                       
                                                                 +--------------------+                                                                                
                                                                 |No Projects:        |                                                                                
                                                                 |                    |                                                                                
                                                                 |                    |                                                                                
                                                                 +--------------------+                                                                                
                                                                             ^                                                                                         
                                                                             |                                                                                         
                                         +--------Is Not Authenticated-------                                                                                          
                                         |                                                                                                                             
                                         |                                                                                                                             
         .-.                            / \                                                                                                                            
        (   )------------------------> |   |                                                                                                                           
         `-'                            \ /                                                                                                                            
                                         V                                                                                                                             
                                         |                                                                                                                             
                                         +---------Is Authenticated----------+                                                                                         
                                                                             v                                                                                         
                                                                   +--------------------+                                                                              
                                                                   |GitHub Projects     |                                                                              
                                                                   |Read/Write/Build    |                                                                              
                                                                   |(Match GH Perms)    |                                                                              
                                                                   +--------------------+                                                                              
                                                                   +--------------------+                                                                              
                                                                   |Boris Projects:     |                                                                              
                                                                   |Read/Write/Build    |                                                                              
                                                                   |                    |                                                                              
                                                                   +--------------------+                                                                              
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
                                                                                                                                                                       
 +---------------------------------------------+                                                                                                                       
 |     Multi Tenant GitHub Authentication      |                                                                                                                       
 +---------------------------------------------+                                                                                                                       
                                                                  +--------------------+                                                                               
                                                                  |Public Projects:    |                               +----------------------------------------------+
                                                                  |Read                |                               | Will Boris Build It?                         |
                                                                  |                    |                               |                                              |
                                                                  +--------------------+                               | Project owned by individual?                 |
                                                                              ^                                        |                                              |
                                                                              |                                        |   Project owner is subscribed?               |
                                          +--------Is Not Authenticated-------                                         |                                              |
                                          |                                                                            |    ~> Then build on commit.                  |
                                          |                                                                            |                                              |
          .-.                            / \                                                                           |   Project contributor is subscribed          |
         (   )------------------------> |   |                                                                          |     AND                                      |
          `-'                            \ /                                                                           |   Project contributor has approved project.  |
                                          V                                                                            |                                              |
                                          |                                                                            |    ~> Then build on commit.                  |
                                          +---------Is Authenticated----------+                                        |                                              |
                                                                              v                                        |   Authenticated user has read access.        |
                                                                              X                                        |     AND                                      |
                                                                             / \                                       |   Authenticated user is subscribed.          |
                                                                       +----|   |----+                                 |                                              |
                                                                       |     \ /     |                                 |    ~> Then build on request.                 |
                                                                       |      V      |                                 |                                              |
                                                      Is Not Subscribed|             |Is Subscribed                    |   Authenticated user has read access.        |
                                                                       |             |                                 |     AND                                      |
                                            +--------------------+     |             |     +--------------------+      |   Authenticated user is subscribed.          |
                                            |Public Projects:    |     |             |     |Public Projects:    |      |     AND                                      |
                                            |Read                | <---+             +---> |Read                |      |   Autheticated user has approved project.    |
                                            |                    |                         |                    |      |                                              |
                                            +--------------------+                         +--------------------+      |    ~> Then build on request.                 |
                                                                                                                       |                                              |
                                            +--------------------+                         +--------------------+      +----------------------------------------------+
                                            |GitHub Projects:    |                         |GitHub Projects     |                                                      
                                            |Read/Write          |                         |Read/Write/Build    |                                                      
                                            |(Match GH Perms)    |                         |(Match GH Perms)    |                                                      
                                            +--------------------+                         +--------------------+                                                      
                                                                                                                                                                       
                                            +--------------------+                         +--------------------+                                                      
                                            |Boris Projects:     |                         |Boris Projects:     |                                                      
                                            |Read/Write          |                         |Read/Write/Build    |                                                      
                                            |                    |                         |                    |                                                      
                                            +--------------------+                         +--------------------+                                                      
```
