-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Selections.Rectangle exposing (..)

{-| RectangleSelection contains the tools to manipule rectangle selections.
-}

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Json.Encode as JE
import Selections.Selection as Sel exposing (Selection)
import Time exposing (Time)


-- MODEL #############################################################


type alias Geometry =
    { pos : Sel.Pos
    , size : Sel.Size
    }


defaultGeometry : Geometry
defaultGeometry =
    { pos = Sel.Pos 0 0
    , size = Sel.Size 0 0
    }


geomFrom2Points : ( Int, Int ) -> ( Int, Int ) -> Geometry
geomFrom2Points ( x1, y1 ) ( x2, y2 ) =
    let
        left =
            min x1 x2

        top =
            min y1 y2

        width =
            abs (x1 - x2)

        height =
            abs (y1 - y2)
    in
        { pos = Sel.Pos left top
        , size = Sel.Size width height
        }


type alias Rectangle =
    { selection : Selection
    , geometry : Geometry
    }


defaultRectangle : Rectangle
defaultRectangle =
    { selection = Sel.defaultSelection
    , geometry = defaultGeometry
    }



-- UPDATE ############################################################


changeStrokeWidth : Float -> Rectangle -> Rectangle
changeStrokeWidth width rect =
    { rect | selection = Sel.changeStrokeWidth width rect.selection }


changeGeometry : ( Int, Int, Int, Int ) -> Rectangle -> Rectangle
changeGeometry ( x, y, width, height ) rect =
    { rect | geometry = Geometry (Sel.Pos x y) (Sel.Size width height) }


update : ( Int, Int ) -> ( Int, Int ) -> Rectangle -> Rectangle
update origin newPos rect =
    { rect | geometry = geomFrom2Points origin newPos }


resetTimings : Rectangle -> Rectangle
resetTimings rect =
    { rect | selection = Sel.resetTimings rect.selection }


setStartTime : Maybe Time -> Rectangle -> Rectangle
setStartTime t rect =
    { rect | selection = Sel.setStartTime t rect.selection }


setStopTime : Maybe Time -> Rectangle -> Rectangle
setStopTime t rect =
    { rect | selection = Sel.setStopTime t rect.selection }



-- VIEW ##############################################################


view : Rectangle -> Svg msg
view rect =
    Svg.rect
        (Sel.selectionAttributes rect.selection
            ++ geometryAttributes rect.geometry
        )
        []


geometryAttributes : Geometry -> List (Svg.Attribute msg)
geometryAttributes geometry =
    [ SvgA.x (toString geometry.pos.x)
    , SvgA.y (toString geometry.pos.y)
    , SvgA.width (toString geometry.size.width)
    , SvgA.height (toString geometry.size.height)
    ]



-- OUTPUTS ##############################################################


object : Rectangle -> JE.Value
object rect =
    JE.object
        [ ( "geometry", geomObject rect.geometry )
        , ( "selection", Sel.selectionObject rect.selection )
        ]


geomObject : Geometry -> JE.Value
geomObject geom =
    JE.object
        [ ( "pos", Sel.posObject geom.pos )
        , ( "size", Sel.sizeObject geom.size )
        ]


pathObject : Rectangle -> JE.Value
pathObject rect =
    let
        -- sides
        left =
            rect.geometry.pos.x

        top =
            rect.geometry.pos.y

        right =
            left + rect.geometry.size.width

        bottom =
            top + rect.geometry.size.height

        -- corners
        top_left =
            Sel.posPathObject <| Sel.Pos left top

        bottom_left =
            Sel.posPathObject <| Sel.Pos left bottom

        bottom_right =
            Sel.posPathObject <| Sel.Pos right bottom

        top_right =
            Sel.posPathObject <| Sel.Pos right top
    in
        JE.object
            [ ( "duration", Sel.maybeTimeObject <| Sel.duration rect.selection.timings )
            , ( "path", JE.list [ top_left, bottom_left, bottom_right, top_right ] )
            ]
