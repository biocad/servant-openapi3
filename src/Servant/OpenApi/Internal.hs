{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE UndecidableInstances #-}
#endif
{-# OPTIONS_GHC -Wno-orphans #-}
module Servant.OpenApi.Internal where

import Prelude ()
import Prelude.Compat

#if MIN_VERSION_servant(0,18,1)
import           Control.Applicative                    ((<|>))
#endif
import           Control.Lens
import           Data.Aeson
import           Data.Foldable              (toList)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.OpenApi               hiding (Header, contentType)
import qualified Data.OpenApi               as OpenApi
import           Data.OpenApi.Declare
import           Data.Proxy
import           Data.Singletons.Bool
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Typeable              (Typeable)
import           GHC.TypeLits
import           Network.HTTP.Media         (MediaType)
import           Servant.API
import           Servant.API.Description    (FoldDescription, reflectDescription)
import           Servant.API.Modifiers      (FoldRequired)
#if MIN_VERSION_servant(0,19,0)
import           Servant.API.Generic        (ToServantApi)
#endif

import           Servant.OpenApi.Internal.TypeLevel.API

-- | Generate a OpenApi specification for a servant API.
--
-- To generate OpenApi specification, your data types need
-- @'ToParamSchema'@ and/or @'ToSchema'@ instances.
--
-- @'ToParamSchema'@ is used for @'Capture'@, @'QueryParam'@ and @'Header'@.
-- @'ToSchema'@ is used for @'ReqBody'@ and response data types.
--
-- You can easily derive those instances via @Generic@.
-- For more information, refer to
-- <http://hackage.haskell.org/package/openapi3/docs/Data-OpenApi.html openapi3 documentation>.
--
-- Example:
--
-- @
-- newtype Username = Username String deriving (Generic, ToText)
--
-- instance ToParamSchema Username
--
-- data User = User
--   { username :: Username
--   , fullname :: String
--   } deriving (Generic)
--
-- instance ToJSON User
-- instance ToSchema User
--
-- type MyAPI = QueryParam "username" Username :> Get '[JSON] User
--
-- myOpenApi :: OpenApi
-- myOpenApi = toOpenApi (Proxy :: Proxy MyAPI)
-- @
class HasOpenApi api where
  -- | Generate a OpenApi specification for a servant API.
  toOpenApi :: Proxy api -> OpenApi

instance HasOpenApi Raw where
  toOpenApi _ = mempty & paths . at "/" ?~ mempty

instance HasOpenApi EmptyAPI where
  toOpenApi _ = mempty

-- | All operations of sub API.
-- This is similar to @'operationsOf'@ but ensures that operations
-- indeed belong to the API at compile time.
subOperations :: (IsSubAPI sub api, HasOpenApi sub) =>
  Proxy sub     -- ^ Part of a servant API.
  -> Proxy api  -- ^ The whole servant API.
  -> Traversal' OpenApi Operation
subOperations sub _ = operationsOf (toOpenApi sub)

-- | Make a singleton OpenApi spec (with only one endpoint).
-- For endpoints with no content see 'mkEndpointNoContent'.
mkEndpoint :: forall a cs hs proxy method status.
  (ToSchema a, AllAccept cs, AllToResponseHeader hs, OpenApiMethod method, KnownNat status)
  => FilePath                                       -- ^ Endpoint path.
  -> proxy (Verb method status cs (Headers hs a))  -- ^ Method, content-types, headers and response.
  -> OpenApi
mkEndpoint path proxy
  = mkEndpointWithSchemaRef (Just ref) path proxy
      & components.schemas .~ defs
  where
    (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty

-- | Make a singletone 'OpenApi' spec (with only one endpoint) and with no content schema.
mkEndpointNoContent :: forall nocontent cs hs proxy method status.
  (AllAccept cs, AllToResponseHeader hs, OpenApiMethod method, KnownNat status)
  => FilePath                                               -- ^ Endpoint path.
  -> proxy (Verb method status cs (Headers hs nocontent))  -- ^ Method, content-types, headers and response.
  -> OpenApi
mkEndpointNoContent path proxy
  = mkEndpointWithSchemaRef Nothing path proxy

-- | Like @'mkEndpoint'@ but with explicit schema reference.
-- Unlike @'mkEndpoint'@ this function does not update @'definitions'@.
mkEndpointWithSchemaRef :: forall cs hs proxy method status a.
  (AllAccept cs, AllToResponseHeader hs, OpenApiMethod method, KnownNat status)
  => Maybe (Referenced Schema)
  -> FilePath
  -> proxy (Verb method status cs (Headers hs a))
  -> OpenApi
mkEndpointWithSchemaRef mref path _ = mempty
  & paths.at path ?~
    (mempty & method ?~ (mempty
      & at code ?~ Inline (mempty
            & content .~ InsOrdHashMap.fromList
              [(t, mempty & schema .~ mref) | t <- responseContentTypes]
            & headers .~ responseHeaders)))
  where
    method               = openApiMethod (Proxy :: Proxy method)
    code                 = fromIntegral (natVal (Proxy :: Proxy status))
    responseContentTypes = allContentType (Proxy :: Proxy cs)
    responseHeaders      = Inline <$> toAllResponseHeaders (Proxy :: Proxy hs)

mkEndpointNoContentVerb :: forall proxy method.
  (OpenApiMethod method)
  => FilePath                      -- ^ Endpoint path.
  -> proxy (NoContentVerb method)  -- ^ Method
  -> OpenApi
mkEndpointNoContentVerb path _ = mempty
  & paths.at path ?~
    (mempty & method ?~ (mempty
      & at code ?~ Inline mempty))
  where
    method               = openApiMethod (Proxy :: Proxy method)
    code                 = 204 -- hardcoded in servant-server

-- | Add parameter to every operation in the spec.
addParam :: Param -> OpenApi -> OpenApi
addParam param = allOperations.parameters %~ (Inline param :)

-- | Add RequestBody to every operations in the spec.
addRequestBody :: RequestBody -> OpenApi -> OpenApi
addRequestBody rb = allOperations . requestBody ?~ Inline rb

-- | Format given text as inline code in Markdown.
markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"

addDefaultResponse404 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse404 pname = setResponseWith (\old _new -> alter404 old) 404 (return response404)
  where
    sname = markdownCode pname
    description404 = sname <> " not found"
    alter404 = description %~ ((sname <> " or ") <>)
    response404 = mempty & description .~ description404

addDefaultResponse400 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse400 pname = setResponseWith (\old _new -> alter400 old) 400 (return response400)
  where
    sname = markdownCode pname
    description400 = "Invalid " <> sname
    alter400 = description %~ (<> (" or " <> sname))
    response400 = mempty & description .~ description400

-- | Methods, available for OpenApi.
class OpenApiMethod method where
  openApiMethod :: proxy method -> Lens' PathItem (Maybe Operation)

instance OpenApiMethod 'GET     where openApiMethod _ = get
instance OpenApiMethod 'PUT     where openApiMethod _ = put
instance OpenApiMethod 'POST    where openApiMethod _ = post
instance OpenApiMethod 'DELETE  where openApiMethod _ = delete
instance OpenApiMethod 'OPTIONS where openApiMethod _ = options
instance OpenApiMethod 'HEAD    where openApiMethod _ = head_
instance OpenApiMethod 'PATCH   where openApiMethod _ = patch

#if MIN_VERSION_servant(0,18,1)
instance HasOpenApi (UVerb method cs '[]) where
  toOpenApi _ = mempty

-- | @since <2.0.1.0>
instance
  {-# OVERLAPPABLE #-}
  ( ToSchema a,
    HasStatus a,
    AllAccept cs,
    OpenApiMethod method,
    HasOpenApi (UVerb method cs as)
  ) =>
  HasOpenApi (UVerb method cs (a ': as))
  where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy (Verb method (StatusOf a) cs a))
      `combineSwagger` toOpenApi (Proxy :: Proxy (UVerb method cs as))
    where
      -- workaround for https://github.com/GetShopTV/swagger2/issues/218
      combinePathItem :: PathItem -> PathItem -> PathItem
      combinePathItem s t = PathItem
        { _pathItemGet = _pathItemGet s <> _pathItemGet t
        , _pathItemPut = _pathItemPut s <> _pathItemPut t
        , _pathItemPost = _pathItemPost s <> _pathItemPost t
        , _pathItemDelete = _pathItemDelete s <> _pathItemDelete t
        , _pathItemOptions = _pathItemOptions s <> _pathItemOptions t
        , _pathItemHead = _pathItemHead s <> _pathItemHead t
        , _pathItemPatch = _pathItemPatch s <> _pathItemPatch t
        , _pathItemTrace = _pathItemTrace s <> _pathItemTrace t
        , _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
        , _pathItemSummary = _pathItemSummary s <|> _pathItemSummary t
        , _pathItemDescription = _pathItemDescription s <|> _pathItemDescription t
        , _pathItemServers = _pathItemServers s <> _pathItemServers t
        }

      combineSwagger :: OpenApi -> OpenApi -> OpenApi
      combineSwagger s t = OpenApi
        { _openApiOpenapi = _openApiOpenapi s <> _openApiOpenapi t
        , _openApiInfo = _openApiInfo s <> _openApiInfo t
        , _openApiServers = _openApiServers s <> _openApiServers t
        , _openApiPaths = InsOrdHashMap.unionWith combinePathItem (_openApiPaths s) (_openApiPaths t)
        , _openApiComponents = _openApiComponents s <> _openApiComponents t
        , _openApiSecurity = _openApiSecurity s <> _openApiSecurity t
        , _openApiTags = _openApiTags s <> _openApiTags t
        , _openApiExternalDocs = _openApiExternalDocs s <|> _openApiExternalDocs t
        }

instance (Typeable (WithStatus s a), ToSchema a) => ToSchema (WithStatus s a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)
#endif

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs, KnownNat status, OpenApiMethod method) => HasOpenApi (Verb method status cs a) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (Verb method status cs (Headers '[] a)))

-- | @since 1.1.7
instance (ToSchema a, Accept ct, KnownNat status, OpenApiMethod method) => HasOpenApi (Stream method status fr ct a) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (Verb method status '[ct] (Headers '[] a)))

instance {-# OVERLAPPABLE #-} (ToSchema a, AllAccept cs, AllToResponseHeader hs, KnownNat status, OpenApiMethod method)
  => HasOpenApi (Verb method status cs (Headers hs a)) where
  toOpenApi = mkEndpoint "/"

-- ATTENTION: do not remove this instance!
-- A similar instance above will always use the more general
-- polymorphic -- HasOpenApi instance and will result in a type error
-- since 'NoContent' does not have a 'ToSchema' instance.
instance (AllAccept cs, KnownNat status, OpenApiMethod method) => HasOpenApi (Verb method status cs NoContent) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (Verb method status cs (Headers '[] NoContent)))

instance (AllAccept cs, AllToResponseHeader hs, KnownNat status, OpenApiMethod method)
  => HasOpenApi (Verb method status cs (Headers hs NoContent)) where
  toOpenApi = mkEndpointNoContent "/"

instance (OpenApiMethod method) => HasOpenApi (NoContentVerb method) where
  toOpenApi =  mkEndpointNoContentVerb "/"

instance (HasOpenApi a, HasOpenApi b) => HasOpenApi (a :<|> b) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy a) <> toOpenApi (Proxy :: Proxy b)

-- | @'Vault'@ combinator does not change our specification at all.
instance (HasOpenApi sub) => HasOpenApi (Vault :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)

-- | @'IsSecure'@ combinator does not change our specification at all.
instance (HasOpenApi sub) => HasOpenApi (IsSecure :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)

-- | @'RemoteHost'@ combinator does not change our specification at all.
instance (HasOpenApi sub) => HasOpenApi (RemoteHost :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)

-- | @'HttpVersion'@ combinator does not change our specification at all.
instance (HasOpenApi sub) => HasOpenApi (HttpVersion :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)

#if MIN_VERSION_servant(0,20,0)
-- | @'WithResource'@ combinator does not change our specification at all.
instance (HasOpenApi sub) => HasOpenApi (WithResource res :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
#endif

-- | @'WithNamedContext'@ combinator does not change our specification at all.
instance (HasOpenApi sub) => HasOpenApi (WithNamedContext x c sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)

instance (KnownSymbol sym, HasOpenApi sub) => HasOpenApi (sym :> sub) where
  toOpenApi _ = prependPath piece (toOpenApi (Proxy :: Proxy sub))
    where
      piece = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToParamSchema a, HasOpenApi sub, KnownSymbol (FoldDescription mods)) => HasOpenApi (Capture' mods sym a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addParam param
    & prependPath capture
    & addDefaultResponse404 tname
    where
      pname = symbolVal (Proxy :: Proxy sym)
      tname = Text.pack pname
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      capture = "{" <> pname <> "}"
      param = mempty
        & name .~ tname
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & required ?~ True
        & in_ .~ ParamPath
        & schema ?~ Inline (toParamSchema (Proxy :: Proxy a))

-- | OpenApi Spec doesn't have a notion of CaptureAll, this instance is the best effort.
instance (KnownSymbol sym, ToParamSchema a, HasOpenApi sub) => HasOpenApi (CaptureAll sym a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (Capture sym a :> sub))

instance (KnownSymbol desc, HasOpenApi api) => HasOpenApi (Description desc :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)
    & allOperations.description %~ (Just (Text.pack (symbolVal (Proxy :: Proxy desc))) <>)

instance (KnownSymbol desc, HasOpenApi api) => HasOpenApi (Summary desc :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)
    & allOperations.summary %~ (Just (Text.pack (symbolVal (Proxy :: Proxy desc))) <>)

instance (KnownSymbol sym, ToParamSchema a, HasOpenApi sub, SBoolI (FoldRequired mods), KnownSymbol (FoldDescription mods)) => HasOpenApi (QueryParam' mods sym a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      param = mempty
        & name .~ tname
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & required ?~ reflectBool (Proxy :: Proxy (FoldRequired mods))
        & in_ .~ ParamQuery
        & schema ?~ Inline sch
      sch = toParamSchema (Proxy :: Proxy a)

instance (KnownSymbol sym, ToParamSchema a, HasOpenApi sub) => HasOpenApi (QueryParams sym a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      param = mempty
        & name .~ tname
        & in_ .~ ParamQuery
        & schema ?~ Inline pschema
      pschema = mempty
        & type_ ?~ OpenApiArray
        & items ?~ OpenApiItemsObject (Inline $ toParamSchema (Proxy :: Proxy a))

instance (KnownSymbol sym, HasOpenApi sub) => HasOpenApi (QueryFlag sym :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      param = mempty
        & name .~ tname
        & in_ .~ ParamQuery
        & allowEmptyValue ?~ True
        & schema ?~ (Inline $ (toParamSchema (Proxy :: Proxy Bool))
                & default_ ?~ toJSON False)

instance (KnownSymbol sym, ToParamSchema a, HasOpenApi sub, SBoolI (FoldRequired mods), KnownSymbol (FoldDescription mods)) => HasOpenApi (Header' mods  sym a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addParam param
    & addDefaultResponse400 tname
    where
      tname = Text.pack (symbolVal (Proxy :: Proxy sym))
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      param = mempty
        & name .~ tname
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & required ?~ reflectBool (Proxy :: Proxy (FoldRequired mods))
        & in_ .~ ParamHeader
        & schema ?~ (Inline $ toParamSchema (Proxy :: Proxy a))

instance (ToSchema a, AllAccept cs, HasOpenApi sub, KnownSymbol (FoldDescription mods)) => HasOpenApi (ReqBody' mods cs a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addRequestBody reqBody
    & addDefaultResponse400 tname
    & components.schemas %~ (<> defs)
    where
      tname = "body"
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
      reqBody = (mempty :: RequestBody)
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        -- ReqBody' is always required, as per the Servant documentation
        & required ?~ True
        & content .~ InsOrdHashMap.fromList [(t, mempty & schema ?~ ref) | t <- allContentType (Proxy :: Proxy cs)]

-- | This instance is an approximation.
--
-- @since 1.1.7
instance (ToSchema a, Accept ct, HasOpenApi sub, KnownSymbol (FoldDescription mods)) => HasOpenApi (StreamBody' mods fr ct a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
    & addRequestBody reqBody
    & addDefaultResponse400 tname
    & components.schemas %~ (<> defs)
    where
      tname = "body"
      transDesc ""   = Nothing
      transDesc desc = Just (Text.pack desc)
      (defs, ref) = runDeclare (declareSchemaRef (Proxy :: Proxy a)) mempty
      reqBody = (mempty :: RequestBody)
        & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
        & content .~ InsOrdHashMap.fromList [(t, mempty & schema ?~ ref) | t <- toList $ contentTypes (Proxy :: Proxy ct)]

#if MIN_VERSION_servant(0,18,2)
instance (HasOpenApi sub) => HasOpenApi (Fragment a :> sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy sub)
#endif

#if MIN_VERSION_servant(0,19,0)
instance (HasOpenApi (ToServantApi sub)) => HasOpenApi (NamedRoutes sub) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy (ToServantApi sub))
#endif

-- =======================================================================
-- Below are the definitions that should be in Servant.API.ContentTypes
-- =======================================================================

class AllAccept cs where
  allContentType :: Proxy cs -> [MediaType]

instance AllAccept '[] where
  allContentType _ = []

instance (Accept c, AllAccept cs) => AllAccept (c ': cs) where
  allContentType _ = contentType (Proxy :: Proxy c) : allContentType (Proxy :: Proxy cs)

class ToResponseHeader h where
  toResponseHeader :: Proxy h -> (HeaderName, OpenApi.Header)

instance (KnownSymbol sym, ToParamSchema a) => ToResponseHeader (Header sym a) where
  toResponseHeader _ = (hname, mempty & schema ?~ hschema)
    where
      hname = Text.pack (symbolVal (Proxy :: Proxy sym))
      hschema = Inline $ toParamSchema (Proxy :: Proxy a)

class AllToResponseHeader hs where
  toAllResponseHeaders :: Proxy hs -> InsOrdHashMap HeaderName OpenApi.Header

instance AllToResponseHeader '[] where
  toAllResponseHeaders _ = mempty

instance (ToResponseHeader h, AllToResponseHeader hs) => AllToResponseHeader (h ': hs) where
  toAllResponseHeaders _ = InsOrdHashMap.insert headerName headerBS hdrs
    where
      (headerName, headerBS) = toResponseHeader (Proxy :: Proxy h)
      hdrs = toAllResponseHeaders (Proxy :: Proxy hs)

instance AllToResponseHeader hs => AllToResponseHeader (HList hs) where
  toAllResponseHeaders _ = toAllResponseHeaders (Proxy :: Proxy hs)
