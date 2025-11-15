--- Spawn Pallets Stacked
-- @author GMNGjoy
-- @copyright 08/12/2024
-- @contact https://github.com/GMNGjoy/FS22_SpawnPalletsStacked
-- @license CC0 1.0 Universal

SpawnPalletsStacked = {}
SpawnPalletsStacked.path = g_currentModDirectory;
SpawnPalletsStacked.modName = g_currentModName;
SpawnPalletsStacked.stackComplete = false;
SpawnPalletsStacked.config = {}

-- default settings
SpawnPalletsStacked.stackedLimitY = 0.75

-- debugging
SpawnPalletsStacked.debugFull = false;
SpawnPalletsStacked.debugStopMultiLayer = false;

SpawnPalletsStacked.noStackPalletType = {
	"METAL",
	"BIRDHOUSE",
	"DOGHOUSE",
	"CATTREE",
}

--- Return the valid xml name for any given xml path;
--- confirms via the store manager & dlc directories to return the actual path.
---@param configName string
---@return string|nil
function SpawnPalletsStacked.getValidXmlName(configName)

	local xmlFilename = configName
	if g_storeManager:getItemByXMLFilename(xmlFilename) then
		return xmlFilename
	end

	xmlFilename = g_modsDirectory..configName
	if g_storeManager:getItemByXMLFilename(xmlFilename) then
		return xmlFilename
	end

	for i = 1, #g_dlcsDirectories do
		local dlcsDir = g_dlcsDirectories[i].path
		xmlFilename = dlcsDir..configName
		if g_storeManager:getItemByXMLFilename(xmlFilename) then
			return xmlFilename
		end
	end

	return nil
end

--- Simple shallow copy method to make sure we copy and not pass a reference
---@param t table
---@return table
function SpawnPalletsStacked.shallowCopy(t)
	local u = { }
	for k, v in pairs(t) do u[k] = v end
	return setmetatable(u, getmetatable(t))
end

--- Simple method to check the length of the array
---@param t table
---@return table
function SpawnPalletsStacked.tableLength(t)
	local length = 0
	for _ in pairs(t) do length = length + 1 end
	return length
end


--- Decimals in FS22 are annoying; shortening them is the only way.
---@param number number incoming float to parse to a smaller float
---@param digitPosition number length to trim to
---@return number
function SpawnPalletsStacked.roundDecimal(number, digitPosition)
	local precision = 10 ^ digitPosition
	number = number + (precision / 2);
	return math.floor(number / precision) * precision
end

--- Get the adjustment for the current building if it exists
---@param xmlFilename string
---@return table
function SpawnPalletsStacked.getAdjustment(xmlFilename)

	-- load in the adjustment which may be used to affect the spawn height
	local thisAdjustment = nil
	for i, adjustment in ipairs(SpawnPalletsStacked.config.adjustments) do
		if thisAdjustment == nil and adjustment.filename == xmlFilename then
			thisAdjustment = adjustment
			break
		end
	end

	-- default the adjustment to an empty array
	if thisAdjustment == nil then
		thisAdjustment = {}
	end

	return thisAdjustment
end

--- Get the max spawn height from either the default, or the override.
---@param adjustment table
---@return number
function SpawnPalletsStacked.getMaxSpawnHeight(adjustment)

	-- load the custom maxSpawnHeight
	local maxSpawnHeight = SpawnPalletsStacked.config.maxSpawnHeight
	if adjustment ~= nil then
		if adjustment.spawnHeight then maxSpawnHeight = adjustment.spawnHeight end
	end

	return maxSpawnHeight
end

--- Get the total number of layers that can spawn within the maxHeight from the spawner
---@param palletHeight number
---@param maxSpawnHeight number
---@return number
function SpawnPalletsStacked.getNumPalletLayers(palletHeight, maxSpawnHeight)
	local numPalletLayers = 0
	local layerOffset = SpawnPalletsStacked.roundDecimal(palletHeight, -1) + 0.1
	local totalHeight = layerOffset

	while totalHeight <= maxSpawnHeight do
		numPalletLayers = numPalletLayers + 1
		totalHeight = totalHeight + layerOffset
	end

	return numPalletLayers
end

--- Get the adjustment for the current building if it exists
---@param palletType string
---@return boolean
function SpawnPalletsStacked.shouldStackPallet(palletType)

	-- load in the adjustment which may be used to affect the spawn height
	local shouldStack = true
	for i, noStackType in ipairs(SpawnPalletsStacked.noStackPalletType) do
		if palletType == noStackType then
			shouldStack = false
		end
	end

	return shouldStack
end

--- Update a single production to spawn stacked pallets as needed.
---@param productionItem table
function SpawnPalletsStacked.updateSingleProduction(productionItem)

	-- get the xml filename and owner
    local xmlFilename = productionItem.configFileName
	local validXmlFilename = SpawnPalletsStacked.getValidXmlName(xmlFilename)

	if not validXmlFilename then return end

    -- allow a bit of debug
	if SpawnPalletsStacked.debugFull then
		printf("---- SPS Production: %s [%s]", productionItem:getName(), validXmlFilename)
	end

    -- get the production's current position
    local productionPosition = productionItem.position
    
    -- Safety check: in FS25 some placeables might not have .position, get from rootNode instead
    if productionPosition == nil then
        local x, y, z = getWorldTranslation(productionItem.rootNode)
        productionPosition = {x = x, y = y, z = z}
    end

    -- get the productions calculated spawn locations
    local specProductionPoint = productionItem.spec_productionPoint.productionPoint
	local palletSpawner = specProductionPoint.palletSpawner

	-- exit if we don't need to continue
	if palletSpawner == nil then
		if SpawnPalletsStacked.debugFull then
			print("---- - SKIP no pallets")
		end
		return
	end

	-- mapping from fillTypeId to pallet
	local outputFillTypePallets = specProductionPoint.outputFillTypeIdsToPallets

	-- specialized spawner that limits spawning items individual spawnPlaces
	if palletSpawner.fillTypeToSpawnPlaces and SpawnPalletsStacked.tableLength(palletSpawner.fillTypeToSpawnPlaces) > 0 then

		if SpawnPalletsStacked.debugFull then
			printf("---- SPS Production: Handling multiple spawn places by fill type for %s", productionItem:getName())
		end

		for fillTypeId, spawnPlaces in pairs(palletSpawner.fillTypeToSpawnPlaces) do
			-- determine the max pallet height from all pallets to spawn
			local pallet = outputFillTypePallets[fillTypeId]
			local palletTypeName = g_fillTypeManager.fillTypes[fillTypeId].name

			if not pallet then
				printf("---- SPS Production: Skipping fillTypeId: %s; No available pallet for %s", fillTypeId, palletTypeName)
			else
				local shouldStack = true
				local palletSize = pallet.size

				if shouldStack and not SpawnPalletsStacked.shouldStackPallet(palletTypeName) then
					if SpawnPalletsStacked.debugFull then
						printf("---- - NOT STACKING! %s", palletTypeName)
					end
					shouldStack = false
				end

				-- round off stupid decimals
				local roundedPalletHeight = SpawnPalletsStacked.roundDecimal(palletSize.height, -1)
				local roundedPalletLength = SpawnPalletsStacked.roundDecimal(palletSize.width, -1)

				-- get the max spawn height from local function
				local adjustment = SpawnPalletsStacked.getAdjustment(validXmlFilename)
				local maxSpawnHeight = SpawnPalletsStacked.getMaxSpawnHeight(adjustment)

				-- save the should stack setting
				adjustment.shouldStack = shouldStack

				-- determine how many layers to spawn; layer 0 is the existing layer
				local numPalletLayers = SpawnPalletsStacked.getNumPalletLayers(roundedPalletHeight, maxSpawnHeight)

				-- public debug
				printf("---- SPS Production: %s Spawner: %s  [doubleUp:%s, stacking:%s]", productionItem:getName(), palletTypeName, adjustment.shouldDoubleUp, adjustment.shouldStack)

				-- call the shared function to update the layers of spawned pallets
				SpawnPalletsStacked.updateSingleSpawner(spawnPlaces, productionPosition.y, numPalletLayers, roundedPalletHeight, roundedPalletLength, adjustment)
			end
		end -- for fillTypeId, spawnPlaces in pairs(palletSpawner.fillTypeToSpawnPlaces)

	else

		if SpawnPalletsStacked.debugFull then
			printf("---- SPS Production: Handling single spawn place for %s", productionItem:getName())
		end

		-- determine the max pallet height from all pallets to spawn
		local maxPalletHeight = 0
		local maxPalletLength = 0
		local shouldStack = true
		for fillTypeId, pallet in pairs(outputFillTypePallets) do
			local palletTypeName = g_fillTypeManager.fillTypes[fillTypeId].name
			local palletSize = pallet.size
			if maxPalletHeight < palletSize.height then
				maxPalletHeight = palletSize.height
			end

			-- transpose length to width because it's backwards
			if maxPalletLength < palletSize.length then
				maxPalletLength = palletSize.length
			end

			if SpawnPalletsStacked.debugFull then
				printf("---- - palletSize %s", palletTypeName)
				DebugUtil.printTableRecursively(palletSize)
			end

			if shouldStack and not SpawnPalletsStacked.shouldStackPallet(palletTypeName) then
				if SpawnPalletsStacked.debugFull then
					printf("---- - NOT STACKING! %s", palletTypeName)
				end
				shouldStack = false
			end
		end

		-- round off stupid decimals
		local roundedPalletHeight = SpawnPalletsStacked.roundDecimal(maxPalletHeight, -1)
		local roundedPalletLength = SpawnPalletsStacked.roundDecimal(maxPalletLength, -1)

		-- get the max spawn height from local function
		local adjustment = SpawnPalletsStacked.getAdjustment(validXmlFilename)
		local maxSpawnHeight = SpawnPalletsStacked.getMaxSpawnHeight(adjustment)

		-- save the should stack setting
		if adjustment.shouldStack ~= shouldStack then
			adjustment.shouldStack = shouldStack
		end

		-- don't doubleUp greenhouses
		if tostring(productionItem.typeName) == "greenhouse" then
			adjustment.shouldDoubleUp = false
		end

		-- determine how many layers to spawn; layer 0 is the existing layer
		local numPalletLayers = SpawnPalletsStacked.getNumPalletLayers(roundedPalletHeight, maxSpawnHeight)

		-- public debug
		printf("---- SPS Production: spawnPlaces %s [doubleUp:%s, stacking:%s]", productionItem:getName(), adjustment.shouldDoubleUp, adjustment.shouldStack)

		-- call the shared function to update the layers of spawned pallets
		SpawnPalletsStacked.updateSingleSpawner(palletSpawner.spawnPlaces, productionPosition.y, numPalletLayers, roundedPalletHeight, roundedPalletLength, adjustment)

	end  -- if palletSpawner.fillTypeToSpawnPlaces
end

--- Update a single husbandry to spawn stacked pallets as needed.
---@param husbandryItem table
---@param isRetry boolean
function SpawnPalletsStacked.updateSingleHusbandry(husbandryItem, isRetry)

	-- get the xml filename and owner
	local xmlFilename = husbandryItem.configFileName
	local validXmlFilename = SpawnPalletsStacked.getValidXmlName(xmlFilename)

	-- exit out of we couldn't find the filename
	if not validXmlFilename then return end

	if SpawnPalletsStacked.debugFull then
		printf("---- Husbandry: %s [%s] (retry: %s)", husbandryItem:getName(), validXmlFilename, tostring(isRetry or false))
	end

	-- get the husbandry name and current position
    local husbandryName = husbandryItem:getName()
	local husbandryPosition = husbandryItem.position
	
	-- Safety check: in FS25 some placeables might not have .position, get from rootNode instead
	if husbandryPosition == nil then
		local x, y, z = getWorldTranslation(husbandryItem.rootNode)
		husbandryPosition = {x = x, y = y, z = z}
	end

	-- skip if the husbandry has no pallets
	if not SpecializationUtil.hasSpecialization(PlaceableHusbandryPallets, husbandryItem.specializations) then
		if SpawnPalletsStacked.debugFull then
			print("---- SKIP - no pallets")
		end
		return
	end

	-- defined local vars
	local specHusbandryPallets = husbandryItem.spec_husbandryPallets
	
	-- FS25 CHANGE: Husbandries now use fillTypeIndexToPalletSpawner instead of a single palletSpawner
	-- They can have multiple spawners for different products (eggs, wool, etc.)
	if not specHusbandryPallets.fillTypeIndexToPalletSpawner then
		if SpawnPalletsStacked.debugFull then
			printf("---- - SKIP: No fillTypeIndexToPalletSpawner for %s", husbandryName)
		end
		return
	end
	
	local hadMissingSpawnPlaces = false
	
	-- Process each spawner for each fill type this husbandry produces
	for fillTypeIndex, palletSpawner in pairs(specHusbandryPallets.fillTypeIndexToPalletSpawner) do
		
		if SpawnPalletsStacked.debugFull then
			printf("---- - Processing fillType: %s", fillTypeIndex)
		end
		
		-- Safety check: verify spawner has spawnPlaces
		if not palletSpawner.spawnPlaces or #palletSpawner.spawnPlaces == 0 then
			if SpawnPalletsStacked.debugFull then
				printf("---- - SKIP fillType %s: No spawnPlaces available", fillTypeIndex)
			end
			hadMissingSpawnPlaces = true
		else
			-- Get pallet info from the spawner
			local pallet = nil
			if palletSpawner.fillTypeIdToPallet and palletSpawner.fillTypeIdToPallet[fillTypeIndex] then
				pallet = palletSpawner.fillTypeIdToPallet[fillTypeIndex]
			end
			
			-- Safety check: if pallet lookup fails, skip this fill type
			if pallet and pallet.size then
				local palletSize = pallet.size

				-- round off stupid decimals
				local roundedPalletHeight = SpawnPalletsStacked.roundDecimal(palletSize.height, -1)
				local roundedPalletLength = SpawnPalletsStacked.roundDecimal(palletSize.length, -1)

				-- get the adjustment if it exists, max spawn height from local function
				local adjustment = SpawnPalletsStacked.getAdjustment(validXmlFilename)
				local maxSpawnHeight = SpawnPalletsStacked.getMaxSpawnHeight(adjustment)

				-- determine how many layers to spawn; layer 0 is the existing layer
				local numPalletLayers = SpawnPalletsStacked.getNumPalletLayers(roundedPalletHeight, maxSpawnHeight)

				-- public debug
				printf("---- SPS Husbandry: %s fillType:%s [doubleUp:%s, stacking:%s]", husbandryName, fillTypeIndex, adjustment.shouldDoubleUp, adjustment.shouldStack)

				-- call the shared function to update the layers of spawned pallets
				SpawnPalletsStacked.updateSingleSpawner(palletSpawner.spawnPlaces, husbandryPosition.y, numPalletLayers, roundedPalletHeight, roundedPalletLength, adjustment)
			else
				if SpawnPalletsStacked.debugFull then
					printf("---- - SKIP fillType %s: Cannot find pallet info", fillTypeIndex)
				end
			end
		end
	end
	
	-- If we had missing spawn places and this isn't a retry, schedule a delayed retry
	if hadMissingSpawnPlaces and not isRetry then
		if SpawnPalletsStacked.debugFull then
			printf("---- - Scheduling delayed retry for %s in %sms", husbandryName, SpawnPalletsStacked.config.retryDelayMs)
		end
		-- Retry after configured delay
		local function retryUpdate()
			SpawnPalletsStacked.updateSingleHusbandry(husbandryItem, true)
		end
		-- Use a timer to retry later
		if g_currentMission then
			g_currentMission:addTimedFunction(retryUpdate, SpawnPalletsStacked.config.retryDelayMs)
		end
	end

end

--- Update a single production to spawn stacked pallets as needed.
---@param beehiveSpawnerItem table
---@param isRetry boolean
function SpawnPalletsStacked.updateSingleBeehiveSpawner(beehiveSpawnerItem, isRetry)

	-- get the xml filename and owner
    local xmlFilename = beehiveSpawnerItem.configFileName
	local validXmlFilename = SpawnPalletsStacked.getValidXmlName(xmlFilename)

	-- exit out of we couldn't find the filename
	if not validXmlFilename then return end

    -- allow a bit of debug
	if SpawnPalletsStacked.debugFull then
		printf("---- SPS BeehiveSpawner: %s [%s] (retry: %s)", beehiveSpawnerItem:getName(), validXmlFilename, tostring(isRetry or false))
	end

    -- get the production's current position
    local productionPosition = beehiveSpawnerItem.position
    
    -- Safety check: in FS25 some placeables might not have .position, get from rootNode instead
    if productionPosition == nil then
        local x, y, z = getWorldTranslation(beehiveSpawnerItem.rootNode)
        productionPosition = {x = x, y = y, z = z}
    end

    -- get the productions calculated spawn locations
    local specBeehiveSpawner = beehiveSpawnerItem.spec_beehivePalletSpawner
	
	-- Safety check: make sure spec exists
	if not specBeehiveSpawner then
		if SpawnPalletsStacked.debugFull then
			printf("---- - SKIP: No spec_beehivePalletSpawner")
		end
		return
	end
	
	local palletSpawner = specBeehiveSpawner.palletSpawner
	
	-- Safety check: make sure palletSpawner exists
	if not palletSpawner then
		if SpawnPalletsStacked.debugFull then
			printf("---- - SKIP: No palletSpawner")
		end
		return
	end
	
	-- Safety check: verify spawner has spawnPlaces
	if not palletSpawner.spawnPlaces or #palletSpawner.spawnPlaces == 0 then
		if SpawnPalletsStacked.debugFull then
			printf("---- - SKIP: No spawnPlaces available")
		end
		
		-- If this isn't a retry, schedule a delayed retry
		if not isRetry then
			if SpawnPalletsStacked.debugFull then
				printf("---- - Scheduling delayed retry for beehive spawner in %sms", SpawnPalletsStacked.config.retryDelayMs)
			end
			-- Retry after configured delay
			local function retryUpdate()
				SpawnPalletsStacked.updateSingleBeehiveSpawner(beehiveSpawnerItem, true)
			end
			-- Use a timer to retry later
			if g_currentMission then
				g_currentMission:addTimedFunction(retryUpdate, SpawnPalletsStacked.config.retryDelayMs)
			end
		end
		return
	end

	-- determine the max pallet height from all pallets to spawn
	local palletType = specBeehiveSpawner.fillType
	local pallet = palletSpawner.fillTypeIdToPallet[palletType]
	
	-- Safety check: if pallet lookup fails, skip this beehive
	if not pallet or not pallet.size then
		if SpawnPalletsStacked.debugFull then
			printf("---- - SKIP: Cannot find pallet info for fillType %s", palletType)
		end
		return
	end
	
	local palletSize = pallet.size

	-- round off stupid decimals
	local roundedPalletHeight = SpawnPalletsStacked.roundDecimal(palletSize.height, -1)
	local roundedPalletLength = SpawnPalletsStacked.roundDecimal(palletSize.length, -1)

	-- get the max spawn height from local function
	local adjustment = SpawnPalletsStacked.getAdjustment(validXmlFilename)
	local maxSpawnHeight = SpawnPalletsStacked.getMaxSpawnHeight(adjustment)

	-- determine how many layers to spawn; layer 0 is the existing layer
	local numPalletLayers = SpawnPalletsStacked.getNumPalletLayers(roundedPalletHeight, maxSpawnHeight)

	-- public debug
	printf("---- SPS BeehiveSpawner: %s fillType:%s [doubleUp:%s, stacking:%s]", beehiveSpawnerItem:getName(), palletType, adjustment.shouldDoubleUp, adjustment.shouldStack)

	-- call the shared function to update the layers of spawned pallets
	SpawnPalletsStacked.updateSingleSpawner(palletSpawner.spawnPlaces, productionPosition.y, numPalletLayers, roundedPalletHeight, roundedPalletLength, adjustment)
end

--- Update a spawner to add more pallets based on pallet height
---@param spawnPlaces table
---@param rootY number
---@param numberOfLayers number how many additional layers to spawn
---@param palletHeight number height of a single pallet
---@param palletLength number length of a single pallet
---@param adjustment table
function SpawnPalletsStacked.updateSingleSpawner(spawnPlaces, rootY, numberOfLayers, palletHeight, palletLength, adjustment)

	if SpawnPalletsStacked.debugFull then
		printf("---- SPS Update Single Spawner [rootY:%s, numberOflayers:%s, palletHeight:%s, palletLength:%s]", rootY, numberOfLayers, palletHeight, palletLength)
	end

	-- when we offset layers, we want to offset by the height of the pallet, plus a gap.
	local layerOffset = palletHeight + SpawnPalletsStacked.config.layerOffset

	-- check for already stacked spawners
	local alreadyHasStackedPallets = false
	local totalRows = 0
	for i, spawnPlace in ipairs(spawnPlaces) do
		totalRows = totalRows + 1
		local spawnDifference = math.abs(spawnPlace.startY - rootY)
		if spawnDifference > SpawnPalletsStacked.stackedLimitY then
			if SpawnPalletsStacked.debugFull then
				printf("---- SPS Already Stacked - difference:%s", spawnDifference)
			end
			alreadyHasStackedPallets = true
		end
	end

	-- allow overrides from config file above
	local doubleUpBack = SpawnPalletsStacked.config.doubleUpShift
	local doubleUpForward = math.abs(doubleUpBack - (palletLength + SpawnPalletsStacked.config.doubleUpGap))
	local shouldDoubleUp = SpawnPalletsStacked.config.shouldDoubleUp
	local shouldStack = true
	if adjustment ~= nil then

		if SpawnPalletsStacked.debugFull then
			printf("---- - Adjustment")
			DebugUtil.printTableRecursively(adjustment)
		end

		if adjustment.shouldStack == false then shouldStack = false end
		if alreadyHasStackedPallets == true then shouldStack = false end
		if adjustment.shouldDoubleUp == false then shouldDoubleUp = adjustment.shouldDoubleUp end
		if adjustment.doubleUpShift then
			doubleUpBack = adjustment.doubleUpShift
			doubleUpForward = math.abs(doubleUpBack - (palletLength + SpawnPalletsStacked.config.doubleUpGap))
		end
		if adjustment.doubleUpGap then doubleUpForward = math.abs(doubleUpBack - (palletLength + adjustment.doubleUpGap)) end

		-- public debug
		printf("---- - ADDING Adjustment; stack: %s double: %s back: %s forward: %s ",
			shouldStack, shouldDoubleUp, doubleUpBack, doubleUpForward)
	end

	-- if the spawners are already stacked, then exit
	if not shouldStack then
		if SpawnPalletsStacked.debugFull then
			printf("---- SPS Should Not Stack!")
		end
		return
	end

	-- setup the spawn places object
	local newSpawnPlaces = {}
	for layer = 0, numberOfLayers, 1 do
		table.insert(newSpawnPlaces, layer, {})
	end

	-- determine if we're going to dynamically double up the row
	local doubleUp = (shouldDoubleUp and totalRows == 1 and spawnPlaces[1].width > SpawnPalletsStacked.config.minWidthToDoubleUp)

	-- if the adjustment.shouldDoubleUp setting is true, this will force a double up even if there enough space.
	if (adjustment ~= nil and adjustment.shouldDoubleUp == true) then
		doubleUp = true
	end

	-- loop through the spawn places doubling them up as needed.
	for i, spawnPlace in ipairs(spawnPlaces) do

		if SpawnPalletsStacked.debugFull then
			printf("---- - Original SpawnPlace #%s: [%s, %s, %s]", i,
				spawnPlace.startX, spawnPlace.startY, spawnPlace.startZ)
		end

		-- set the currentY from the original spawn place
		local currentY = spawnPlace.startY

		-- stack the original spawn place if needed.
		if shouldStack and not SpawnPalletsStacked.debugStopMultiLayer then
			for layer = 1, numberOfLayers, 1 do
				local newSpawnPlaceX = SpawnPalletsStacked.shallowCopy(spawnPlace)
				currentY = currentY + layerOffset
				newSpawnPlaceX.startY = currentY
				if doubleUp then
					newSpawnPlaceX.startX = spawnPlace.startX - (spawnPlace.dirPerpX * doubleUpBack)
					newSpawnPlaceX.startZ = spawnPlace.startZ - (spawnPlace.dirPerpZ * doubleUpBack)
				end
				table.insert(newSpawnPlaces[layer], newSpawnPlaceX)
			end
		end

		if doubleUp then
			if SpawnPalletsStacked.debugFull then
				print("---- - We're doubling up, adding another spawn row!")
			end

			-- reset the currentY
			currentY = spawnPlace.startY

			-- dupe the first layer, move it forward
			local newSpawnPlace2 = SpawnPalletsStacked.shallowCopy(spawnPlace)
			newSpawnPlace2.startX = spawnPlace.startX + (spawnPlace.dirPerpX * doubleUpForward)
			newSpawnPlace2.startZ = spawnPlace.startZ + (spawnPlace.dirPerpZ * doubleUpForward)
			table.insert(newSpawnPlaces[0], newSpawnPlace2)

			-- dupe the rest of the layers again, moved forward.
			if shouldStack and not SpawnPalletsStacked.debugStopMultiLayer then
				for layer = 1, numberOfLayers, 1 do
					local newSpawnPlace2X = SpawnPalletsStacked.shallowCopy(spawnPlace)
					currentY = currentY + layerOffset
					newSpawnPlace2X.startY = currentY
					if doubleUp then
						newSpawnPlace2X.startX = spawnPlace.startX + (spawnPlace.dirPerpX * doubleUpForward)
						newSpawnPlace2X.startZ = spawnPlace.startZ + (spawnPlace.dirPerpZ * doubleUpForward)
					end
					table.insert(newSpawnPlaces[layer], newSpawnPlace2X)
				end
			end

			-- move the existing row
			spawnPlace.startX = spawnPlace.startX - (spawnPlace.dirPerpX * doubleUpBack)
			spawnPlace.startZ = spawnPlace.startZ - (spawnPlace.dirPerpZ * doubleUpBack)
			spawnPlaces[i] = spawnPlace
		end

	end

	-- add the layers recursively
	for layer, layerSpawnPlaces in pairs(newSpawnPlaces) do
		for i, newSpawnPlace in ipairs(layerSpawnPlaces) do
			table.insert(spawnPlaces, newSpawnPlace)
			if SpawnPalletsStacked.debugFull then
				printf("---- - Added Spawn Place: %s layer: %s at: [%s, %s, %s]", i , layer,
				newSpawnPlace.startX, newSpawnPlace.startY, newSpawnPlace.startZ)
			end
		end
	end

	if SpawnPalletsStacked.debugFull then
		print("---- -- stack complete --")
	end
end

--- Update all the pallet spawners across the map currently being loaded.
function SpawnPalletsStacked:updateAllPalletSpawners()
    -- Only update it once!
    if SpawnPalletsStacked.stackComplete then
        return
    end

	if SpawnPalletsStacked.debugFull then
    	printf("---- SPS: updatePalletSpawners")
	end

    -- Loop through all of the placeables on the map.
	if g_currentMission ~= nil and g_currentMission.placeableSystem and g_currentMission.placeableSystem.placeables then

		-- loop through all the husbandries, and stack the pallets
		for v=1, #g_currentMission.placeableSystem.placeables do

			local thisPlaceable = g_currentMission.placeableSystem.placeables[v]
			local typeName = tostring(thisPlaceable.typeName)

			if string.find(typeName, "Husbandry") then

                if SpawnPalletsStacked.debugFull then
					printf("---- SPS: found a husbandry - %s", thisPlaceable:getName())
				end
				SpawnPalletsStacked.updateSingleHusbandry(thisPlaceable)

            elseif typeName == "productionPoint" or typeName == "productionPointWardrobe" or typeName == "greenhouse" then

                if SpawnPalletsStacked.debugFull then
					printf("---- SPS: found a production - %s", thisPlaceable:getName())
				end
				SpawnPalletsStacked.updateSingleProduction(thisPlaceable)

			elseif typeName == "beehivePalletSpawner" then

                if SpawnPalletsStacked.debugFull then
					printf("---- SPS: found a beehivePalletSwawner - %s", thisPlaceable:getName())
				end
				SpawnPalletsStacked.updateSingleBeehiveSpawner(thisPlaceable)

			else
				if SpawnPalletsStacked.debugFull then
					printf("---- SPS: skipping: %s type: %s", thisPlaceable:getName(), typeName)
				end
            end
		end
    end

	-- Listen for items being placed, and update spawners accordingly.
	PlaceableProductionPoint.onFinalizePlacement = Utils.appendedFunction(PlaceableProductionPoint.onFinalizePlacement, SpawnPalletsStacked.onFinalizePlacement)
	PlaceableHusbandryAnimals.onFinalizePlacement = Utils.appendedFunction(PlaceableHusbandryAnimals.onFinalizePlacement, SpawnPalletsStacked.onFinalizePlacement)
	
	-- Hook beehive pallet spawners if the class exists
	if PlaceableBeehivePalletSpawner ~= nil then
		PlaceableBeehivePalletSpawner.onFinalizePlacement = Utils.appendedFunction(PlaceableBeehivePalletSpawner.onFinalizePlacement, SpawnPalletsStacked.onFinalizePlacement)
	end

	-- mark complete so we don't run again
	SpawnPalletsStacked.stackComplete = true
end

--- Hook into the completion of a placed item, so we can update the spawners
---@param savegame any
function SpawnPalletsStacked:onFinalizePlacement(savegame)
	-- context: self is the placed item

	if SpawnPalletsStacked.debugFull then
    	printf("---- SPS: Update spawners for new: %s", self:getName())
	end

	-- Check for the specialization of the placed item
	if SpecializationUtil.hasSpecialization(PlaceableHusbandryPallets, self.specializations) then

		SpawnPalletsStacked.updateSingleHusbandry(self)

	elseif SpecializationUtil.hasSpecialization(PlaceableProductionPoint, self.specializations) then

		SpawnPalletsStacked.updateSingleProduction(self)
		
	elseif self.spec_beehivePalletSpawner ~= nil then
	
		SpawnPalletsStacked.updateSingleBeehiveSpawner(self)

	end
end

--- initialize the script at the appropriate time.
function SpawnPalletsStacked:init()

	print('-- SpawnPalletsStacked: Initialize')

	source(g_currentModDirectory.."xmlConfigLoader.lua")
	SpawnPalletsStacked.config = XmlConfigLoader.init()

	DebugUtil.printTableRecursively(SpawnPalletsStacked.config)

	if (SpawnPalletsStacked.config.debugMode == true) then
		SpawnPalletsStacked.debugFull = true
		print('-- SpawnPalletsStacked: DEBUG-ON')
	end

	if (SpawnPalletsStacked.config.debugStopMultiLayer == true) then
		SpawnPalletsStacked.debugStopMultiLayer = true
		print('-- SpawnPalletsStacked: DEBUG-STOP-STACKING')
	end

    -- Update the spawners already on the map only after the mission is fully loaded
	Mission00.loadMission00Finished = Utils.appendedFunction(Mission00.loadMission00Finished, SpawnPalletsStacked.updateAllPalletSpawners);

end

SpawnPalletsStacked:init()