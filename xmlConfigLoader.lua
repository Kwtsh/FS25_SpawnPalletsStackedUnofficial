--- XML Config Loader
-- @author GMNGjoy
-- @copyright 08/12/2024
-- @contact https://github.com/GMNGjoy/FS22_SpawnPalletsStacked
-- @license CC0 1.0 Universal

XmlConfigLoader = {}
XmlConfigLoader.path = g_currentModDirectory;
XmlConfigLoader.modName = g_currentModName;
XmlConfigLoader.loadComplete = false;
XmlConfigLoader.debugMode = false;
XmlConfigLoader.debugStopMultiLayer = false;

-- configuration files and loading states
XmlConfigLoader.internalConfig = {};
XmlConfigLoader.internalConfigLoaded = false;
XmlConfigLoader.userConfig = {};
XmlConfigLoader.userConfigLoaded = false;

-- defaults for existing settings
XmlConfigLoader.maxSpawnHeight = 2.0
XmlConfigLoader.layerOffset = 0.05
XmlConfigLoader.shouldDoubleUp = true
XmlConfigLoader.doubleUpShift = 0.7
XmlConfigLoader.doubleUpGap = 1.1
XmlConfigLoader.minWidthToDoubleUp = 3.0
XmlConfigLoader.retryDelayMs = 2000

-- xmlConfigFiles
XmlConfigLoader.internalConfigFile = "xml/internalConfig.xml"
XmlConfigLoader.defaultConfigFile = "xml/defaultConfig.xml"
XmlConfigLoader.userConfigFile = "modSettings/SpawnPalletsStacked.xml"
XmlConfigLoader.xmlTag = 'spawnPalletsStacked'

--- Initialize the loader
---@return table
function XmlConfigLoader.init()

	print("---- SPS Loader: INIT internal and user configurations")

	-- setup the xml schema
	XmlConfigLoader.initXml()

	-- import the internal config first
	local internalFilename = Utils.getFilename(XmlConfigLoader.internalConfigFile, XmlConfigLoader.path)
	XmlConfigLoader.internalConfig = XmlConfigLoader.importConfig(internalFilename)
	XmlConfigLoader.internalConfigLoaded = true

	-- determine the proper path for the user's settings file
	local userSettingsFile = Utils.getFilename(XmlConfigLoader.userConfigFile, getUserProfileAppPath())

	local N = 0
	local spawnConfig = {}
	if fileExists(userSettingsFile) then

		print("---- SPS Loader: IMPORT user production overrides")
		XmlConfigLoader.userConfig = XmlConfigLoader.importConfig(userSettingsFile)
		XmlConfigLoader.userConfigLoaded = true

		spawnConfig = XmlConfigLoader.mergeSettings()

	else

		print("---- SPS Loader: CREATING user settings file")
		local defaultSettingsFile = Utils.getFilename(XmlConfigLoader.defaultConfigFile, XmlConfigLoader.path)
		copyFile(defaultSettingsFile, userSettingsFile, false)

		spawnConfig = XmlConfigLoader.internalConfig
	end

	return spawnConfig
end

--- Merge the adjustment objects that have been loaded.
---@return table
function XmlConfigLoader.mergeAdjustments()

	if not XmlConfigLoader.userConfig.adjustments then
		return XmlConfigLoader.internalConfig.adjustments
	end

	local mergedAdjustments = {}
	local adjustmentIndex = 0
	for i, adjustment in pairs(XmlConfigLoader.userConfig.adjustments) do
		local existingAdjustment = nil
		for j, internalAdjustment in pairs(XmlConfigLoader.internalConfig.adjustments) do
			if adjustment.filename == internalAdjustment.filename then
				existingAdjustment = internalAdjustment
			end
		end

		-- merge the existing one and this one
		if existingAdjustment ~= nil then
			local mergedAdjustment = SpawnPalletsStacked.shallowCopy(existingAdjustment)
			for _k, _val in pairs(mergedAdjustment) do
				if adjustment[_k] ~= _val then
					mergedAdjustment[_k] = adjustment[_k]
				end
			end
			for _k, _val in pairs(adjustment) do
				if not mergedAdjustment[_k] then
					mergedAdjustment[_k] = adjustment[_k]
				end
			end
			table.insert(mergedAdjustments, adjustmentIndex, mergedAdjustment)
		else
			table.insert(mergedAdjustments, adjustmentIndex, adjustment)
		end

		-- update the index iterator
		adjustmentIndex = adjustmentIndex + 1
	end

	for i, internalAdjustment in pairs(XmlConfigLoader.internalConfig.adjustments) do
		local existingMergedAdjustment = nil
		for j, mergedAdjustment in pairs(mergedAdjustments) do
			if internalAdjustment.filename == mergedAdjustment.filename then
				existingMergedAdjustment = mergedAdjustment
			end
		end

		-- as long as it hasn't already been added, add it.
		if not existingMergedAdjustment then
			table.insert(mergedAdjustments, adjustmentIndex, internalAdjustment)

			-- update the index iterator
			adjustmentIndex = adjustmentIndex + 1
		end
	end

	return mergedAdjustments
end

--- Merge the existing settings objects that have been loaded.
---@return table
function XmlConfigLoader.mergeSettings()
	local mergedConfig = SpawnPalletsStacked.shallowCopy(XmlConfigLoader.internalConfig)

	for _k,_val in pairs(mergedConfig) do
		if _k == 'adjustments' then
			_val = XmlConfigLoader.mergeAdjustments()
		elseif XmlConfigLoader.userConfig[_k] then
			_val = XmlConfigLoader.userConfig[_k]
		end
		mergedConfig[_k] = _val
	end

	return mergedConfig
end


--- Initiaze the XML file configuration
function XmlConfigLoader.initXml()

	XmlConfigLoader.xmlSchema = XMLSchema.new(XmlConfigLoader.xmlTag)
	XmlConfigLoader.xmlSchema:register(XMLValueType.FLOAT, XmlConfigLoader.xmlTag..".settings.maxSpawnHeight", "Sets the height to which pallets can spawn", XmlConfigLoader.maxSpawnHeight)
	XmlConfigLoader.xmlSchema:register(XMLValueType.FLOAT, XmlConfigLoader.xmlTag..".settings.layerOffset", "How much of a gap is created between layers", XmlConfigLoader.layerOffset)
	XmlConfigLoader.xmlSchema:register(XMLValueType.BOOL,  XmlConfigLoader.xmlTag..".settings.shouldDoubleUp", "Should the script automatically double up single row spawn points?", XmlConfigLoader.shouldDoubleUp)
	XmlConfigLoader.xmlSchema:register(XMLValueType.FLOAT, XmlConfigLoader.xmlTag..".settings.doubleUpShift", "How far to 'shift the first row to accomodate two rows", XmlConfigLoader.doubleUpShift)
	XmlConfigLoader.xmlSchema:register(XMLValueType.FLOAT, XmlConfigLoader.xmlTag..".settings.doubleUpGap", "The spacing between the rows when a double up is applied", XmlConfigLoader.doubleUpGap)
	XmlConfigLoader.xmlSchema:register(XMLValueType.FLOAT, XmlConfigLoader.xmlTag..".settings.minWidthToDoubleUp", "Minimum spawn width for pallets to be doubled up", XmlConfigLoader.minWidthToDoubleUp)
	XmlConfigLoader.xmlSchema:register(XMLValueType.BOOL, XmlConfigLoader.xmlTag..".settings.debugMode", "Turn debugMode on for additional log output", false)
	XmlConfigLoader.xmlSchema:register(XMLValueType.BOOL, XmlConfigLoader.xmlTag..".settings.debugStopMultiLayer", "Disable stacking feature as a debug step", false)
	XmlConfigLoader.xmlSchema:register(XMLValueType.INT, XmlConfigLoader.xmlTag..".settings.retryDelayMs", "Delay in milliseconds before retrying spawn place detection", XmlConfigLoader.retryDelayMs)

	local adjustmentKey = XmlConfigLoader.xmlTag..".adjustments.adjustment(?)"
	local adjustmentSchemas = {
		[1] = { ["schema"] = XmlConfigLoader.xmlSchema, ["key"] = adjustmentKey },
	}
	for _, s in ipairs(adjustmentSchemas) do
		s.schema:register(XMLValueType.STRING, s.key.."#filename", "Vehicle config file xml full path - used to identify supported vehicles", nil)
		s.schema:register(XMLValueType.FLOAT,  s.key.."#spawnHeight", "Sets the height to which pallets can spawn", nil)
		s.schema:register(XMLValueType.FLOAT,  s.key.."#layerOffset",  "How much of a gap is created between layers", nil)
		s.schema:register(XMLValueType.BOOL,   s.key.."#shouldDoubleUp", "Should the script automatically double up single row spawn points?", nil)
		s.schema:register(XMLValueType.BOOL,   s.key.."#shouldStack", "Should we stack for this particular building?", nil)
		s.schema:register(XMLValueType.FLOAT,  s.key.."#doubleUpShift", "How far to 'shift the first row to accomodate two rows", nil)
		s.schema:register(XMLValueType.FLOAT,  s.key.."#doubleUpGap", "The spacing between the rows when a double up is applied", nil)
	end

end

--- Initiaze the a specified xmlFilename as a config
---@param xmlFilename string
---@return table
function XmlConfigLoader.importConfig(xmlFilename)
	local loadedConfig = {}
	local xmlFile = XMLFile.load("xmlFile", xmlFilename, XmlConfigLoader.xmlSchema)

	if xmlFile ~= 0 then

		printf("---- SPS Loader: LOAD config file [%s]", xmlFilename)

		loadedConfig.maxSpawnHeight = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.maxSpawnHeight", XmlConfigLoader.maxSpawnHeight)
		loadedConfig.layerOffset = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.layerOffset", XmlConfigLoader.layerOffset)
		loadedConfig.shouldDoubleUp = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.shouldDoubleUp", XmlConfigLoader.shouldDoubleUp)
		loadedConfig.doubleUpShift = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.doubleUpShift", XmlConfigLoader.doubleUpShift)
		loadedConfig.doubleUpGap = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.doubleUpGap", XmlConfigLoader.doubleUpGap)
		loadedConfig.minWidthToDoubleUp = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.minWidthToDoubleUp", XmlConfigLoader.minWidthToDoubleUp)
		loadedConfig.debugMode = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.debugMode", XmlConfigLoader.debugMode)
		loadedConfig.debugStopMultiLayer = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.debugStopMultiLayer", XmlConfigLoader.debugStopMultiLayer)
		loadedConfig.retryDelayMs = xmlFile:getValue(XmlConfigLoader.xmlTag..".settings.retryDelayMs", XmlConfigLoader.retryDelayMs)

		local internalAdjustments = {}

		local i = 0
		while true do
			local configKey = string.format(XmlConfigLoader.xmlTag..".adjustments.adjustment(%d)", i)

			if not xmlFile:hasProperty(configKey) then
				break
			end

			-- init a new object
			local adjustment = {}

			-- set the filename based off of the loaded validXmlFilename
			adjustment.filename = xmlFile:getValue(configKey.."#filename")

			-- set the config values from the xml
			adjustment.spawnHeight = xmlFile:getValue(configKey.."#spawnHeight", nil)
			adjustment.layerOffset = xmlFile:getValue(configKey.."#layerOffset", nil)
			adjustment.shouldDoubleUp = xmlFile:getValue(configKey.."#shouldDoubleUp", nil)
			adjustment.shouldStack = xmlFile:getValue(configKey.."#shouldStack", nil)
			adjustment.doubleUpShift = xmlFile:getValue(configKey.."#doubleUpShift", nil)
			adjustment.doubleUpGap = xmlFile:getValue(configKey.."#doubleUpGap", nil)

			table.insert(internalAdjustments, i, adjustment)

			-- increment i to loop
			i = i + 1

		end  -- end while true

		-- save the internal adjustments table
		loadedConfig.adjustments = internalAdjustments

	end

	xmlFile:delete()
	return loadedConfig
end