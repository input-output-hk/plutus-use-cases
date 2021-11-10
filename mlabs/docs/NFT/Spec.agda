module NFT.Spec where

  open import NFT.Cardano.Builtins
  open import NFT.Predefined.Types
  open import Agda.Builtin.Bool
  open import Agda.Builtin.Maybe
  open import Agda.Builtin.List

  {-
    # Description
    The following description is a pseudo-formal specification/model of the NFT
    Application.
    
    The purpose of the NFT Application is to allow individuals called Authors to
    mint and sell the ownership of Digital Content (refered to as Artwork) to
    other individuals called Buyers. The Application allows the Author to
    maintainin part ownership of the Artwork via a pre-specified amount of
    shares. Upon the re-selling of the artwork the Author would receive an
    equivalent amount of money to the shares retained in the artwork.

    The administration of the Application is decentralised, through the use of a
    Governance system. This allows the opperation logic of the application to
    change over time, and allows the ownership/decisions of the application to
    be shared with other users in a manner similar to a publicly listed
    companies - users that own Governance in the application can vote to decide
    on the future of the application.
  -}
  
  {-
    ## The Epoch System

    An Application Epoch is a period of time in which the application behaves in
    a predefined manner. In the Current Epoch the NFT Application will have
    epoch specific logic: NFT logic, governance logic, epoch Voting Logic, App Income
    distribution Logic. The epoch system allows the App owners (governance token
    holders) to change the logic of any of the epoch specific logic.

  -}
  data Status : Set where
    Active : Status
    Passed : Status
    Genesis : Status
  
  data Logic : Set where
  
  record Behaviour : Set where
    field
      NFTLogic : Logic
      
  data Epoch : Status → Set where
    current  : Epoch Passed → Behaviour → Epoch Active 
    passed   : Behaviour → Epoch Passed

  today = Epoch Active
  
  {-
    ## The NFT Application
  -}
  
  data DigitalContent : Set where
    Art : BuiltinString → DigitalContent
    𝕖   : DigitalContent

  data Actor : Set where
    Author : Actor
    Admin  : Actor
    Owner  : Actor
    Buyer  : Actor
  
  data Id (a : Actor) : Set where
    onChainId : Address → Id a
    
  {-

    ----------------------------------------------------------------------------  
    # Application Properties 
    
    The Application has the properties:
    
    1. A Buyer has a guarantees that the Author has listed the the Digital
    Content only once on the NFT Application, and that the Digital Content is
    not a counterfeit (has been produced by the Author).

    2. An Author listing the Artwork (minting an NFT) has a guarantee that the
    content cannot be forged by third parties on the application. Furthermore,
    the Author's shares, and Authorship claim cannot be changed, in subsequent
    resells. Finally that the Author, upon each subsequent re-selling receives,
    would receive their share of the winning Bid .

    3. A Seller has a guarantee of selling only to bids higher than the listed
    price, or of retaining the ownership of the Artwork indefinitely if is not
    for sell.

  -}

  postulate InformationAbout : Set → Set

  data Share : Set where
    AuthorShare : Share
    OwnerShare  : Share 
  
  data Property  : Set₁ where
    Unique       : Property
    Owns         : Set → Property
    ElementOf    : Set → Property
    Paid         : Share → Property
    Immutable    : Property
    ForSale      : Property
    ¬            : Property → Property
    AbleToModify : Property → Property
    Price        : Property
    Positive     : Set → Property

  data Changes : Property → Set where
  
  data NFT (a : Set ) : Set where
    mintAct : a → NFT a 
  
  postulate _is_ : Set → Property → Set
  
  data Endpoint : Set where
    MintEndpoint : Endpoint
    BuyEndpoint  : Endpoint
    SetPriceEndpoint : Endpoint

  data EndpointProperty : Endpoint → Set where 
    mintIs : Id Author → DigitalContent → NFT DigitalContent is Unique
                                        → NFT (InformationAbout (Id Author)) is Immutable
                                        → Id Author is Owns (NFT DigitalContent)
                                        → Id Author is AbleToModify Price
                                        → Id Author is AbleToModify ForSale
                                        → Id Owner is AbleToModify Price
                                        → Id Owner is AbleToModify ForSale
                                        → Id Owner is Owns (NFT DigitalContent)
                                        → EndpointProperty MintEndpoint
    
    buyIs : Id Buyer → NFT DigitalContent → NFT DigitalContent is ForSale
                                          → NFT DigitalContent is Unique
                                          → Id Author is Paid AuthorShare
                                          → Id Owner  is Paid OwnerShare
                                          → EndpointProperty BuyEndpoint
    
    setPriceIs : Id Owner → NFT DigitalContent → Id Owner is AbleToModify ForSale
                                               → Id Owner is AbleToModify Price
                                               → EndpointProperty SetPriceEndpoint
  
  {- 

    The NFT Application opperates according to the following description: 

    The Application's Address hosts all the minted NFTs (tokens). Upon the
    selling or re-selling of an NFT, the token remains at the App Address; only
    the information attached to the token (the utxo's Datum) is changed. At any
    given point, all the correctly minted tokens are guaranteed to be consistent
    and correct.
    
    The implementation strategy adopted for the Application is done via the use
    of an Associative Linked List. Each list node holds a unique Artwork
    NFT. The list is sorted via the artwork's unique hash, with every Node
    (except the terminal node) pointing to the next node in the list. Each node
    points towards a node containing a larger 𝔸Token, with the Head of the list
    containing the smallest 𝔸Token - that of the empty string.

  -}
  -- A Proof Token is a unique NFT per Application, and allow the Script to
  -- validate only transactions on correctly minted (authentic) NFTs.
  data ℙToken : Set  where
    𝕡token : AssetClass → ℙToken

  -- An Artwork token is a Unique NFT representing the proof of uniqueness of
  -- the artwork in the context of the App.
  data 𝔸Token : ( BuiltinString → DigitalContent ) → Set where
    𝕒Token : DigitalContent → 𝔸Token Art

  -- A Hashed piece of information.
  postulate Hashed : Set → Set 

  -- An NFTid is what we identify each artwork by. 
  data NFTid : Set where
    nftId : Hashed DigitalContent → NFTid

  -- A Larger piece of information. 
  data Larger (a : Set) : Set where
    _>_ : a → a → Larger a 

  data Pointer (a : Set) : Set where
    Next     : Larger a → Pointer a
    Terminal : Pointer a 

  -- InfoNFT is the datum of each utxo
  record InfoNft : Set where
    field
      NFT-Id          : NFTid 
      NFT-Author      : Id Author
      NFT-AuthorShare : Share 
      NFT-Price       : Maybe Integer
      NFT-Owner       : Id Owner

  postulate
    Minted : Set → Set 

  -- Constantly Empty DigitalContent
  Const-𝕖 : (BuiltinString → DigitalContent)
  Const-𝕖 = λ _ → 𝕖
  
  data AppListElement : Set where
    Head : ℙToken  → 𝔸Token Const-𝕖 → Pointer         (𝔸Token Art)  → AppListElement
    Node : InfoNft → 𝔸Token Art     → Pointer (Larger (𝔸Token Art)) → AppListElement
  
  {-
    ----------------------------------------------------------------------------
    # App Endpoints
  
    ## Initialisation
    
    The Application's NFT Minting Policy is parametrised with the AppInstance
    which consists of : an Application Unique Token (a "one-shot" unique token,
    that cannot be re-minted), the Application's Address, and a list of Admins.
  -}
  
  record AppInstance : Set where
    field
      App'Address : Address
      App'Admins  : List (Id Admin)
      App'Token   : ℙToken
  
  postulate
    -- The parametrised Minting Policy uses a unique token to create a Minting
    -- Policy.
    ParametrisedMintingPolicy : AppInstance → MintingPolicy

    -- The Unique Token is guaranteed by the Plutus One-Shot Contract.
    AppUniqueToken : AssetClass  

  -- The Application minting policy 
  AppMintingPolicy : MintingPolicy
  AppMintingPolicy = ParametrisedMintingPolicy initConfig 
    where
      postulate 
        appAddress : Address
        admins     : List (Id Admin)

      -- The Initial app configuration is provided at the instantiation of the
      -- application.
      initConfig = record
        { App'Address = appAddress
        ; App'Admins  = admins
        ; App'Token   = 𝕡token AppUniqueToken
        }
  
  {-

    The Admin Initialises the Application by minting the Application Unique
    Token and sending it to the Application Address, thus creating the Head of
    the Associative Linked List.

  -}
  postulate
    PayTo : Address → Set → Set
    _and_ : Set → Set → Set
    _or_  : Set → Set → Set

  record InitialiseApp : Set where
    field 
      appInstance : (AppInstance is Unique) is Immutable
      admin       : Id Admin is ElementOf AppInstance
      initTx      : Tx is Unique
      uniqueToken : Id Admin
                    → PayTo AppAddress (UtxoWith (Minted ℙToken) and UtxoWith (Minted (𝔸Token Const-𝕖)))
                    → Tx 

  -- Submiting a transaction
  data MakeTx (a : Set) (b : Actor ) : Set where
    submitTx : a → MakeTx a b

  postulate 
    initApp : InitialiseApp → MakeTx InitialiseApp Admin
  
    
  {-
  --------------------------------------------------------------------------------
    # Minting 
    
    The Application's Minting Policy can verify both on-chain and off-chain if
    the minted NFT is unique.

    Minting a new token is the equivalent of creating an 𝔸Token that can be
    inserted in the Application's Linked List. The conditions in which this can
    happen are:

      1. There is an Application Head or Node containing a smaller 𝔸Token with a
      Terminal Pointer.

      2. There is an Application Head or Node containing a smaller 𝔸Token
      pointing towards a larger 𝔸Token than the 𝔸Token to be inserted.
  
    -----
    The Author must provide :

      1. An eUTXo providing the above information (Proof of uniqueness).

      2. The NFTinformation.

      3. The Application ℙToken.

    -----
    A Valid Transaction must:

      1. Pay back to the AppAddress the ℙToken together with its associated
      𝔸Token unchanged (in one utxo).

      2. Update the ProofTk's pointer to point to the Minted 𝔸Token, and submit
      it to the Application Address.

      3. Submit the Minted 𝔸Token.

  -}
  
  record MintTx : Set where
    field 
     ContentAuthor : Id Author
     ContentInfo   : InfoNft
     Content       : DigitalContent
     ProofTk       : PayTo AppAddress (UtxoWith (Minted (𝔸Token Art)) or UtxoWith (Minted (𝔸Token Const-𝕖))) → Tx
     ProofList     : PayTo AppAddress (UtxoWith (ℙToken and (𝔸Token Const-𝕖))) → Tx
  
  postulate
    mintToken : MintTx → MakeTx MintTx Author 
  
  {-
  --------------------------------------------------------------------------------
    # Set Price 

    The Set price end-point allows the Owner to set the price of an NFT, or to
    list/unlist it as for sale.

    -----
    The Owner needs to provide:

    1. The NFTid of the Minted Token

    2. The new price

    3. The UTXO containing the proof token of the application (HEAD).

    -----
    For a Valid Tx:

    1. The HEAD is paid back with both 𝔸Token and ℙToken unchanged

    2. The Minted ℙToken is paid back together with the update information

    3. No other information is changed/submitted.
  
  -}
  
  postulate
    Updated : Set → Set
  
  record SetPriceTx : Set where
    field 
     𝔸TokenOwner : Id Owner
     𝔸TokenId    : NFTid 
     ListProof   : PayTo AppAddress (UtxoWith (ℙToken and 𝔸Token Const-𝕖)) → Tx
     newPrice    : Maybe Integer
     NFTProof    : PayTo AppAddress (UtxoWith (Updated (𝔸Token Art))) → Tx 

  postulate
    setPrice : SetPriceTx → MakeTx SetPriceTx Owner 

  {-
  --------------------------------------------------------------------------------
    # Buy 

    The Buy end-point allows the Buyer to buy an NFT, while paying both the
    Author and the Owner. In the same transaction, the Buyer sets the new-price,
    or unlists the 𝔸Token.

    -----
    The Buyer needs to provide:

    1. The NFTid of the Minted 𝔸Token.

    2. The new price.

    3. The UTXO containing the proof token of the application (HEAD).

    4. The Bid is high enough to pay the listed price. 

    -----
    For a Valid Tx:

    1. The HEAD is paid back with both 𝔸Token and ℙToken unchanged.

    2. The Bought Minted ℙToken is paid back together with the update information.

    3. No other information is changed/submitted.

    4. The Author is paid  their share.

    5. The Owner is paid their share.
  
  -}  
  postulate
    PayAda : Share → Set → Set

  data utxo : Set where 
  
  record StdTx : Set where
    field
      inputs  : utxo
      outputs : utxo

  
  
  record BuyTx : Set where
    field
     buyer           : Id Buyer
     pay𝔸TokenOwner  : UtxoWith (PayAda OwnerShare  (Id Owner )) → Tx
     pay𝔸TokenAuthor : UtxoWith (PayAda AuthorShare (Id Author)) → Tx
     𝔸TokenId        : NFTid 
     ListProof       : PayTo AppAddress (UtxoWith (ℙToken and 𝔸Token Const-𝕖)) → Tx
     newPrice        : Maybe Integer
     NFTProof        : PayTo AppAddress (UtxoWith (Updated (𝔸Token Art))) → Tx 

  postulate
    buy : BuyTx → MakeTx BuyTx Buyer
  
  {-

  --------------------------------------------------------------------------------
  Technical Specifications

  ## Size limitations
  
    - Make note of the Size Limitations of the transactions:
    https://cardano.stackexchange.com/questions/4124/what-is-the-size-limit-of-transactions-metadata

    - Each Script has a minimum of 3kb - not more than 3 Scripts can be used 
  -}
