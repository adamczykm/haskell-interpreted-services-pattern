
This file is written in literate haskell mode and contains a presentation
of purely functional architectural pattern for larger-scale applications.
Its aim is to provide a way of writing application that is compoosable, modular,
and detangles semantically different aspect of program, such as database access
and logging from each other and from high-level domain logic.
Moreover, when applied, the patterns enables clean (almost without any existing
code modifications) replacing or mocking parts of the applications, which in turn
allows to easy create different application configurations, for another low-level
communication protocols, unit-testing or different storage providers.
I also like the concept of limited effects. The user of limited effects is restricted
to work with general effect interface and cannot use concrete implementation.
With limited effects we can make safe assumptions that our code is in fact
operating on restricted set of effects and not hiding additional calls to any other
arbitral effects with, say, liftIO.

The entire idea is based on the attempt to combine patterns described in great articles
by John A De Goes:
- http://degoes.net/articles/modern-fp
- http://degoes.net/articles/modern-fp-part-2
and Matt Parsons:
- http://www.parsonsmatt.org/2016/07/14/rank_n_classy_limited_effects.html
and heavily inspired by them and probably many others I came along while exploring
arcanas of functional programming.

The pattern is experimental and not tested in real-life applications.
I was more of exploring the possibilities, rather than implementing a thought-out
and fully understood concept. I haven't checked it for performance neither.
If you see some mistakes and/or ways to improve my solution - I'll be glad to hear
from you.
With these disclaimers in mind, let's dive in.

This are required language extensions..

> {-# LANGUAGE TemplateHaskell   #-}
> {-# LANGUAGE KindSignatures    #-}
> {-# LANGUAGE RankNTypes        #-}
> {-# LANGUAGE ConstraintKinds   #-}
> {-# LANGUAGE TypeOperators     #-}
> {-# LANGUAGE TypeFamilies      #-}
> {-# LANGUAGE DataKinds         #-}
> {-# LANGUAGE GADTs             #-}
> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE AllowAmbiguousTypes  #-}
> {-# LANGUAGE DeriveFunctor  #-}


>
> module Lib( test1, test2, test3 ) where
>
> import           Control.Monad.IO.Class
> import           Data.Vinyl
> import           Data.Singletons.TH
> import           Control.Monad.Reader
> import           Control.Monad.Free
> import           Control.Monad.Freer
> import           Control.Monad.Freer.Internal
> import           Data.Maybe (fromMaybe)
> import           Data.IORef
> import           Control.Lens         hiding (Identity)


The core concept of the pattern are application services and interpreters.
Services encapsulate and provide application with abstract languages for expressing
effects related to some aspects of application logic and are in fact interpreters
of these languages.

> data Service = LoggingService | UserService | UnrestrictedIOCalls

The last one may be used when prototyping before effects are aggregated in some
sensible service.

Let's say we're using following services abstract interfaces:


> class Monad m => MonadLog m where
>   logM :: String -> m ()

> type User = Int
>
> class Monad m => MonadUser m where
>  createUser :: User -> m ()
>  getUsers :: Maybe (User -> Bool) -> m [User]
>

As I mentioned a service is in fact just an interpreter for provided top-level language
into some other lower-level language. Let's express this in a type
(we'll need ContraintKinds and RankNTypes extension for this):


> newtype InterpreterTo c n = InterpreterTo (forall a. Monad n => (forall m. c m => m a) -> n a)

With rank n types we can force interpreter to accept only generic computation using exclusively
given effect interface.

To make using limited effects more convenient I'm using extensible records (with vinyl) which will require
some type level functions (and hence TypeFamilies extension):

(TypeOperators extension makes it more readable)


> type family ServicesFam m (s :: Service) :: * where
>   ServicesFam g 'LoggingService = MonadLog `InterpreterTo` g
>   ServicesFam g 'UserService =  MonadUser `InterpreterTo` g
>   ServicesFam g 'UnrestrictedIOCalls =  MonadIO `InterpreterTo` g
>

> newtype Attr f g = Attr { _unAttr :: ServicesFam f g}
>
> (=::) :: (sing f -> ServicesFam g f -> Attr g f)
> (=::) _ = Attr
>
> makeLenses ''Attr
> genSingletons [ ''Service ]
>
> type Services eff = Rec (Attr eff) '[ 'LoggingService, 'UserService, 'UnrestrictedIOCalls ]

Thanks to such implementation it would be possible to create function restricted to
any subset of services.

Let's introduce the Application type and then explain a bit.

> type AllServices = '[ 'LoggingService, 'UserService, 'UnrestrictedIOCalls ]
> type SafeServices = '[ 'LoggingService, 'UserService ]
> type ServiceFun ss r = forall m. (Monad m, ss ⊆ (AllServices :: [Service])) => ReaderT (Rec (Attr m) ss) m r
> newtype App a = App { unApp :: ServiceFun AllServices a }

So the application is just a computation returning some value of given type, forced to operate
in a generic monad monad interface, but with access to services, the use of which, can result
in interpretation to any concrete monadic interface.

By this moment we are able to build our application using abstract service languages.
It's not very convenient and boiler-plate free unfortunately.
I've come up only with some simple helper functions. It is a bit unsatisfying as
every call to a distinct service effect results in using one of the following:


> logService :: forall m ss r. (Monad m, 'LoggingService ∈ ss) => (forall n. MonadLog n => n r) -> ReaderT (Rec (Attr m) ss) m r
> logService sf = do
>   rss <- view (rlens SLoggingService) <$> ask
>   lift $ f . _unAttr $ rss
>     where
>       f ::  MonadLog `InterpreterTo` m -> m r
>       f (InterpreterTo intepret) = intepret sf


> userService :: forall m ss r. (Monad m, 'UserService ∈ ss) => (forall n. MonadUser n => n r) -> ReaderT (Rec (Attr m) ss) m r
> userService sf = do
>   rss <- view (rlens SUserService) <$> ask
>   lift $ f . _unAttr $ rss
>     where
>       f ::  MonadUser `InterpreterTo` m -> m r
>       f (InterpreterTo interpret) = interpret sf


> unrestrictedIO :: forall m ss r. (Monad m, 'UnrestrictedIOCalls ∈ ss) => (forall n. MonadIO n => n r) -> ReaderT (Rec (Attr m) ss) m r
> unrestrictedIO sf = do
>   rss <- view (rlens SUnrestrictedIOCalls) <$> ask
>   lift $ f . _unAttr $ rss
>     where
>       f ::  MonadIO `InterpreterTo` m -> m r
>       f (InterpreterTo interpret) = interpret sf

I have tried to unify this into a one function using more type-level functions, but could get vinyl cooperating.

Let's continue with what we've already worked out and try to create our application.

> exampleApp1 :: App ()
> exampleApp1 = App main
>   where
>     main :: ServiceFun AllServices ()
>     main = do
>       users <- userService $ getUsers Nothing
>       logService $ mapM_ (logM . show) users
>       unrestrictedIO $ liftIO $ mapM_ print users
>

This is how does writing code with interpreted service looks like.

We could then compose our code of more calls to functions of type `ServiceFun AllServices ()`
but then we wouldn't not restrict calee to only necessary effects.
We could for example want to exclude UnrestrictedIOCalls from part of our code.
Just to be sure that NOTHING unexpected is happening there.

This is how I've managed to do it:

> sub :: (Monad m, ss ⊆ (AllServices :: [Service])) => ReaderT (Rec (Attr n) ss) m r -> ReaderT (Rec (Attr n) AllServices) m r
> sub = withReaderT rcast

> exampleApp2 :: App ()
> exampleApp2 = App main
>   where
>     main :: ServiceFun AllServices ()
>     main = do
>       users <- sub safeMain
>       unrestrictedIO $ liftIO $ mapM_ print users
>

>     safeMain :: ServiceFun SafeServices [User]
>     safeMain = do
>           users <- userService $ mapM_ createUser [1..10] >> getUsers (Just (> 5))
>           logService $ mapM_ (logM . show) users
>           return users

The above code is somewhat cluttered with auxilliary type-level manipulating functions at every call to a service
or another service function. Optimally we would like something like:

exampleApp2 :: App ()
exampleApp2 = App main
  where
    main = do
      users <- safeMain
      liftIO $ mapM_ print users

    safeMain = do
          users <- getUsers Nothing
          mapM_ (logM . show) users
          return users

But I haven't yet found a way to implement it.

At this point we've already written our application using abstract services. Now we need to provide concrete implementation
for interpreters. Chosen solutions allows for a great freedom. We can use existing monads or transformers, we can also opt
for Free monad or even Freer if that's what we prefer. This is how it would look like:


> productionUnrestrictedIOService :: MonadIO `InterpreterTo` IO
> productionUnrestrictedIOService = InterpreterTo id


Now using Free:

> data LogF a = Log String a
>   deriving(Functor)
>
> instance MonadLog (Free LogF) where
>  logM str = liftF $ Log str ()

Let's define a convenience operator for expressing natural transformation:

> infixr 0 -->
> type f --> g = forall x. f x -> g x

Actual interpretation is happening here:

> logFAsIO :: LogF --> IO
> logFAsIO (Log str x) = putStrLn ("Log: " ++ str) >> return x

> productionStdoutLogginService :: MonadLog `InterpreterTo` IO
> productionStdoutLogginService = InterpreterTo $ \x -> foldFree logFAsIO x

Last service will be defined with Freer:

> data UserApi a where
>   CreateUser :: User -> UserApi ()
>   GetUsers :: Maybe (User -> Bool) -> UserApi [User]

For demonstrationa purposes it's used as production service :)

> type UserDb = IORef [User]

> instance MonadUser (Eff '[UserApi]) where
>    createUser u = send (CreateUser u)
>    getUsers p = send (GetUsers p)

> userApiAsIORefReader :: Eff '[UserApi] --> ReaderT UserDb IO
> userApiAsIORefReader (Val x) = return x
> userApiAsIORefReader (E u q) = case extract u of
>               (CreateUser user) -> do
>                  ref <- ask
>                  usrs <- lift $ readIORef ref
>                  lift $ writeIORef ref (user : usrs)
>                  userApiAsIORefReader (qApp q ())
>
>               (GetUsers p) -> do
>                  usrs <- ask >>= lift . readIORef
>                  userApiAsIORefReader (qApp q (filter (fromMaybe (const True) p) usrs))


> productionIORefUserService :: UserDb -> MonadUser `InterpreterTo` IO
> productionIORefUserService userDb = InterpreterTo $ \x -> runReaderT (userApiAsIORefReader x) userDb

As soon as we define concrete interpreters for all services we can prepare service record.
Let's assume that th

> createProductionServices :: IO (Services IO)
> createProductionServices = do
>   ref <- newIORef []
>   return $ (SLoggingService =:: productionStdoutLogginService)
>         :& (SUserService         =:: productionIORefUserService ref)
>         :& (SUnrestrictedIOCalls =:: productionUnrestrictedIOService)
>         :& RNil

We're almost there. We still need a functionn that will run our application using given services:

> runApp :: (Monad m) => App a -> Services m -> m a
> runApp = runReaderT . unApp

That was straightforward. Now let's test our example apps.

> test1 :: IO ()
> test1 = createProductionServices >>= runApp exampleApp1

> test2 :: IO ()
> test2 = createProductionServices >>= runApp exampleApp2

And as a remainder - entire "client side".

> test3 :: IO ()
> test3 = createProductionServices >>= runApp (App $ sub safeMain)
>   where
>     safeMain :: ServiceFun SafeServices ()
>     safeMain = do
>           users <- userService $ mapM_ createUser [1..10] >> getUsers (Just (> 5))
>           logService $ logM "Added some users" >> mapM_ (logM . show) users
>           users2 <- userService $ mapM_ createUser [1..20] >> getUsers (Just (> 5))
>           logService $ logM "Added more users" >> mapM_ (logM . show) users2
>           return ()

Summary [TODO]
It's worth to mention that interpreters compose both vertically and horizontally which means that with
"interpreted service" architecture we
