#! /bin/sh

match() {
	echo `find ./ -name "$1" -print0 | xargs -0 ls`
}

walk=$(match "walk_*.png")
dance=$(match "dance_*.png")

animate-frames \
	--spritesheet figure.png \
	--image "data/figure.png" \
	--metadata figure.yaml \
	--animation Walk $walk \
	--animation Dance $dance \
	--yaml \
	--fps 24
