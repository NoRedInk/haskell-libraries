module Filterable
  ( ListWidget,
    init,
    cons,
    toListWidget,
    setListWidget,
    filter,
    reset,
  )
where

import qualified Brick.Widgets.List as ListWidget
import qualified Data.Vector as Vector
import qualified Prelude

data ListWidget name a = ListWidget
  { listWidget :: ListWidget.List name a,
    original :: Vector.Vector a,
    name :: name
  }

init :: name -> ListWidget name a
init name =
  ListWidget
    { listWidget = ListWidget.list name Prelude.mempty 1,
      original = Vector.empty,
      name
    }

toListWidget :: ListWidget name a -> ListWidget.List name a
toListWidget = listWidget

cons :: a -> ListWidget name a -> ListWidget name a
cons x listWidgetList =
  listWidgetList
    { original = Vector.cons x (original listWidgetList),
      listWidget = ListWidget.listInsert 0 x (listWidget listWidgetList)
    }

reset :: ListWidget name a -> ListWidget name a
reset listWidgetList =
  listWidgetList
    { listWidget =
        ListWidget.list (name listWidgetList) (original listWidgetList) 1
    }

setListWidget :: ListWidget name a -> ListWidget.List name a -> ListWidget name a
setListWidget listWidgetList listWidget =
  listWidgetList {listWidget}

filter :: (a -> Bool) -> ListWidget name a -> ListWidget name a
filter f listWidgetList =
  listWidgetList
    { listWidget =
        ListWidget.list
          (name listWidgetList)
          (Vector.filter f (original listWidgetList))
          1
    }
