-------------------------------------------------------------------------------
-- Title      : Side-Channel protected Ascon variants
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ascon.vhdl
-- Author     : Hannes Gross
-- Company    : Graz University of Technology
-- Created    : 2016-11-24
-- Last update: 2016-11-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-24  1.0      hgross	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ascon_pkg.all;
-------------------------------------------------------------------------------

entity ascon is

  generic (
    -- see Ascon package for configuration
    ASCON           : ascon_t := VARIANT;  
    SBOX_INSTANCES  : integer := PARALLEL_SBOXES;
    IMPLICIT_AFFINE : string  := IMPLICIT_AFFINE
    );

  port (
    ClkxCI               : in  std_logic;
    RstxBI               : in  std_logic;
    -- Data Inputs
    KeyxDI               : in  t_shared_key_var(D downto 0);
    DataxDI              : in  t_shared_data   (D downto 0);
    NoncexDI             : in  std_logic_vector(127 downto 0);
    -- Command Inputs
    StartEncryptionxSI   : in  std_logic;
    StartDecryptionxSI   : in  std_logic;
    AssociatedDataRDYxSI : in  std_logic;
    PTCTDataRDYxSI       : in  std_logic;
    FinalizexSI          : in  std_logic;
    TagReceivedxSI       : in  std_logic;
    -- Fresh random Z masks
    Z0xDI                : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z1xDI                : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z2xDI                : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z3xDI                : in  random_t(SBOX_INSTANCES-1 downto 0);
    Z4xDI                : in  random_t(SBOX_INSTANCES-1 downto 0);
    -- Outputs
    BusyxSO              : out std_logic;
    DataxDO              : out t_shared_data(D downto 0);
    DataReadyxSO         : out std_logic;
    TagxDO               : out t_shared_tag (D downto 0)
    );

end entity ascon;

-------------------------------------------------------------------------------

architecture str of ascon is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  -- Ascon's FSM
  signal FSMxDN, FSMxDP : ascon_fsm_t;

  -- 4-bit Round counter
  signal RoundCounter : integer range 0 to 15;
  signal doCountxS    : std_logic;
  signal RoundConstxS : std_logic_vector(3 downto 0);

  -- Status and control registers for correct handling of data
  signal isEncryptionxSN          : std_logic;
  signal isEncryptionxSP          : std_logic;
  signal addedKeyAtInitxSN        : std_logic;
  signal addedKeyAtInitxSP        : std_logic;
  signal addedDomainSeparationxSN : std_logic;
  signal addedDomainSeparationxSP : std_logic;

  -- Control signals to state
  signal doInitializeStatexS       : std_logic;
  signal doRoundTransfromationxS   : std_logic;
  signal isFirstRoundOfTransformxS : std_logic;
  -- ... from state
  signal doIncrementRoundCounterxS : std_logic;
  signal StateBusyxS               : std_logic;
begin  -- architecture str

  -----------------------------------------------------------------------------
  -- Status and control registers
  -----------------------------------------------------------------------------
  -- register process
  status_control_reg_p: process (ClkxCI, RstxBI) is
  begin  -- process status_control_reg-P
    if RstxBI = '0' then                -- asynchronous reset (active low)
      isEncryptionxSP          <= '0';
      addedKeyAtInitxSP        <= '0';
      addedDomainSeparationxSP <= '0';
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      isEncryptionxSP          <= isEncryptionxSN;
      addedKeyAtInitxSP        <= addedKeyAtInitxSN;
      addedDomainSeparationxSP <= addedDomainSeparationxSN;
    end if;
  end process status_control_reg_p;
  
  -----------------------------------------------------------------------------
  -- Round Counter and signaling of first round of transformation
  -----------------------------------------------------------------------------
  round_counter_p : process (ClkxCI, RoundCounter, RstxBI) is
  begin  -- process round_counter_p
    if RstxBI = '0' then                -- asynchronous reset (active low)
      RoundCounter              <= 0;
      isFirstRoundOfTransformxS <= '0';
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      -- Reset on IDLE
      if(FSMxDP = IDLE) then            
        RoundCounter              <= 0;
        isFirstRoundOfTransformxS <= '0';
      else
        -- Count up
        if (doCountxS = '1') then
          -- Overflow
          if (RoundCounter = 12) then
            -- Finalize?
            if FSMxDN = FINALIZE then
              RoundCounter <= 0;
            else
              -- Ascon128a requires more rounds for transformation
              if (ASCON = Ascon128a) then
                RoundCounter <= 4;
              else
                RoundCounter <= 6;
              end if;  -- Ascon128a?
            end if;  -- Finalize?
            
            -- Signalize that this is the first round!
            isFirstRoundOfTransformxS <= '1';
            
          else  -- not an overflow --> increment
            RoundCounter              <= RoundCounter + 1;
            isFirstRoundOfTransformxS <= '0';
          end if;  -- Overflow
        end if; -- Count up
      end if; -- Reset on IDLE
    end if; -- Clock handling
    
    -- Convert counter value to 4-bit bit vector
    RoundConstxS <= std_logic_vector(to_unsigned(RoundCounter, 4));
  end process round_counter_p;
  
  -----------------------------------------------------------------------------
  -- Ascon's FSM
  -----------------------------------------------------------------------------
  ascon_fsm_p : process (AssociatedDataRDYxSI, FSMxDP, FinalizexSI,
                         PTCTDataRDYxSI, RoundCounter, StartDecryptionxSI,
                         StartEncryptionxSI, StateBusyxS, TagReceivedxSI,
                         addedDomainSeparationxSP, addedKeyAtInitxSP,
                         doIncrementRoundCounterxS, isEncryptionxSP,
                         isFirstRoundOfTransformxS) is
  begin  -- process ascon_fsm_p
    -- Register defaults --> previous state
    FSMxDN                   <= FSMxDP;  
    isEncryptionxSN          <= isEncryptionxSP;
    addedKeyAtInitxSN        <= addedKeyAtInitxSP;
    addedDomainSeparationxSN <= addedDomainSeparationxSP;

    -- Control signals defaults
    doInitializeStatexS     <= '0';
    doRoundTransfromationxS <= '0';
    BusyxSO                 <= '1'; -- busy per default
    
    -- State controls counter per default
    doCountxS               <= doIncrementRoundCounterxS;

    -----------------------
    -- State transitions --
    -----------------------
    case FSMxDP is
      -- *** IDLE ***
      when IDLE =>
        if (StartEncryptionxSI = '1') or (StartDecryptionxSI = '1') then
          FSMxDN              <= INIT;
        end if;
              
      -- *** INIT ***
      when INIT =>
        -- End of init?
        if (RoundCounter = 12) then
          if AssociatedDataRDYxSI = '1' then
            FSMxDN <= ASSOCIATED_DATA;
          elsif PTCTDataRDYxSI = '1' then
            FSMxDN <= PTCT_DATA;
          elsif (FinalizexSI = '1') then
            FSMxDN <= FINALIZE;
          end if;
        end if;

      -- *** ASSOCIATED_DATA ***
      when ASSOCIATED_DATA =>
        -- Repeat or proceed?
        if (RoundCounter = 12) then
          if PTCTDataRDYxSI = '1' then
            FSMxDN <= PTCT_DATA;
          elsif (FinalizexSI = '1') then
            FSMxDN <= FINALIZE;
          end if;
        end if;

      -- *** PTCT_DATA ***
      when PTCT_DATA =>
        -- Repeat or proceed?
        if (RoundCounter = 12) and (FinalizexSI = '1') then
            FSMxDN <= FINALIZE;
        end if;
        
      -- *** FINALIZE ***
      when FINALIZE =>
        if RoundCounter = 12 then
          FSMxDN <= READ_TAG;
        end if;

      -- *** READ_TAG ***
      when READ_TAG =>
        if TagReceivedxSI = '1' then
          FSMxDN <= IDLE;
        end if;       
        
      -- default
      when others => null;
    end case; -- State transitions

    ------------------
    -- Output logic --
    ------------------
    case FSMxDP is
      -- *** IDLE ***
      when IDLE =>
        -- Reset on IDLE
        addedKeyAtInitxSN        <= '0';
        addedDomainSeparationxSN <= '0';
        BusyxSO                  <= '0';
        doInitializeStatexS      <= '1';
        -- Memorize if encryption or decryption was selected
        isEncryptionxSN          <= StartEncryptionxSI;
        
      -- *** INIT ***
      when INIT =>
        -- End of transformation?
        if (RoundCounter = 12) then
          if AssociatedDataRDYxSI = '1' or PTCTDataRDYxSI = '1' or FinalizexSI = '1' then
            doCountxS <= '1';
          end if;       
          -- Busy only when State is busy
          BusyxSO <= StateBusyxS; 
        else
          doRoundTransfromationxS <= '1';
        end if;

      -- *** ASSOCIATED_DATA ***
      when ASSOCIATED_DATA =>
        -- set if key was already added to state after init
        if not(isFirstRoundOfTransformxS = '1') then
          addedKeyAtInitxSN <= '1'; 
        end if;

        -- Repeat or proceed?
        if (RoundCounter = 12) then
          if AssociatedDataRDYxSI = '1' or PTCTDataRDYxSI = '1' or FinalizexSI = '1' then
            doCountxS <= '1';
          end if;       
          -- Busy only when State is busy
          BusyxSO <= StateBusyxS; 
        else
          doRoundTransfromationxS <= '1';
        end if;

      -- *** PTCT_DATA ***
      when PTCT_DATA =>
        -- set if key was already added to state after init
        if not(isFirstRoundOfTransformxS = '1') then
          addedKeyAtInitxSN <= '1';
          addedDomainSeparationxSN <= '1';
        end if;
        
        -- Repeat or proceed?
        if (RoundCounter = 12) then
          if PTCTDataRDYxSI = '1' or (FinalizexSI = '1') then
            doCountxS <= '1';
          end if;
          -- Busy only when State is busy
          BusyxSO <= StateBusyxS;
        else
          doRoundTransfromationxS <= '1';
        end if;
        
      -- *** FINALIZE ***
      when FINALIZE =>
        -- Finished?
        if not(RoundCounter = 12) then        
          doRoundTransfromationxS <= '1';
        end if;

      -- *** READ_TAG ***
      when READ_TAG =>
        -- Busy only when State is busy
        BusyxSO <= StateBusyxS;
        
      -- default
      when others => null;
    end case; -- Output logic
  end process ascon_fsm_p;

  -- Register process for FSM
  fsm_reg_p: process (ClkxCI, RstxBI) is
  begin  -- process fsm_reg_p
    if RstxBI = '0' then                -- asynchronous reset (active low)
      FSMxDP <= IDLE;
    elsif ClkxCI'event and ClkxCI = '1' then  -- rising clock edge
      FSMxDP <= FSMxDN;
    end if;
  end process fsm_reg_p;

  -----------------------------------------------------------------------------
  -- State module for Ascon128 or Ascon128a
  -----------------------------------------------------------------------------
  ascon128_g: if ASCON = Ascon128 generate
    ascons_state: entity work.state
    generic map (
      DATA_BLOCK_SIZE => 64,
      KEY_SIZE        => 128,
      ROUNDS_A        => 12,
      ROUNDS_B        => 6,
      SBOX_INSTANCES  => SBOX_INSTANCES,
      IMPLICIT_AFFINE => IMPLICIT_AFFINE,
      PIPELINED       => "yes")
    port map (
      ClkxCI                     => ClkxCI,
      RstxBI                     => RstxBI,
      KeyxDI                     => KeyxDI,
      NoncexDI                   => NoncexDI,
      DataxDI                    => DataxDI,
      RoundConstxDI              => RoundConstxS,
      Z0xDI                      => Z0xDI,
      Z1xDI                      => Z1xDI,
      Z2xDI                      => Z2xDI,
      Z3xDI                      => Z3xDI,
      Z4xDI                      => Z4xDI,
      DoInitializeStatexSI       => doInitializeStatexS,
      DoRoundTransfromationxSI   => doRoundTransfromationxS,
      isEncryptionxSI            => isEncryptionxSP,
      addedKeyAtInitxSI          => addedKeyAtInitxSP,
      addedDomainSeparationxSI   => addedDomainSeparationxSP,
      doIncrementRoundCounterxSO => doIncrementRoundCounterxS,
      isFirstRoundOfTransformxSI => isFirstRoundOfTransformxS,
      FSMxDI                     => FSMxDP,
      DataxDO                    => DataxDO,
      DataReadyxSO               => DataReadyxSO,
      TagxDO                     => TagxDO,
      StateBusyxSO               => StateBusyxS);
  end generate ascon128_g;

  ascon128a_g: if ASCON = Ascon128a generate
    ascons_state: entity work.state
    generic map (
      DATA_BLOCK_SIZE => 128,
      KEY_SIZE        => 128,
      ROUNDS_A        => 12,
      ROUNDS_B        => 8,
      SBOX_INSTANCES  => SBOX_INSTANCES,
      IMPLICIT_AFFINE => IMPLICIT_AFFINE,
      PIPELINED       => "yes")
    port map (
      ClkxCI                     => ClkxCI,
      RstxBI                     => RstxBI,
      KeyxDI                     => KeyxDI,
      NoncexDI                   => NoncexDI,
      DataxDI                    => DataxDI,
      RoundConstxDI              => RoundConstxS,
      Z0xDI                      => Z0xDI,
      Z1xDI                      => Z1xDI,
      Z2xDI                      => Z2xDI,
      Z3xDI                      => Z3xDI,
      Z4xDI                      => Z4xDI,
      DoInitializeStatexSI       => doInitializeStatexS,
      DoRoundTransfromationxSI   => doRoundTransfromationxS,
      isEncryptionxSI            => isEncryptionxSP,
      addedKeyAtInitxSI          => addedKeyAtInitxSP,
      addedDomainSeparationxSI   => addedDomainSeparationxSP,
      doIncrementRoundCounterxSO => doIncrementRoundCounterxS,
      isFirstRoundOfTransformxSI => isFirstRoundOfTransformxS,
      FSMxDI                     => FSMxDP,
      DataxDO                    => DataxDO,
      DataReadyxSO               => DataReadyxSO,
      TagxDO                     => TagxDO,
      StateBusyxSO               => StateBusyxS);
  end generate ascon128a_g;
  
end architecture str;

-------------------------------------------------------------------------------
