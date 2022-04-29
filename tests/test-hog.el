;;; ../Sync/dotfiles/doom.d/lisp/hog-emacs/tests/test-hog.el -*- lexical-binding: t; -*-

(require 'hog)
(require 'ert)
(require 'projectile)

(ert-deftest test-get-project-xml ()

  (should (equal
           (expand-file-name (hog--get-project-xml "oh_ge11"))
           (expand-file-name (concat (projectile-project-root) "/Projects/oh_ge11/oh_ge11.ppr"))))

  (should (equal
           (expand-file-name (hog--get-project-xml "test"))
           (expand-file-name (concat (projectile-project-root) "/Projects/test/test.xpr")))))

(ert-deftest test-parse-project-ppr ()
  (should (equal 1 1)))

(ert-deftest test-parse-project-xml ()
  (should (equal
           (hog--parse-project-xml 'test)
           '(("ctrl_lib"
              ("registers/FW_INFO_PKG.vhd"
               "registers/FW_INFO_map.vhd"
               "registers/MGT_PKG.vhd"
               "registers/MGT_map.vhd"
               "registers/READOUT_BOARD_PKG.vhd"
               "registers/READOUT_BOARD_map.vhd"))
             ("etl_test_fw"
              ("src/utils/clock_strobe.vhd"
               "src/readout_board/components_pkg.vhd"
               "src/readout_board/types_pkg.vhd"
               "src/readout_board/control.vhd"
               "src/utils/counter.vhd"
               "src/wrappers/lpgbt_pkg.vhd"
               "src/utils/prbs_any.vhd"
               "src/gbt_ic/gbt_ic_rx.vhd"
               "src/gbt_ic/sc_wrapper.vhd"
               "src/wrappers/lpgbt_link_wrapper.vhd"
               "src/utils/frame_aligner.vhd"
               "src/utils/fifo_sync.vhd"
               "src/readout_board/wishbone_fifo_reader.vhd"
               "src/readout_board/pattern_checker.vhd"
               "src/readout_board/readout_board.vhd"
               "src/mgt/xlx_ku_mgt_ip_reset_synchronizer.vhd"
               "src/mgt/xlx_ku_mgt_10g24.vhd"
               "src/utils/fifo_async.vhd"
               "src/utils/frequency_counter.vhd"
               "src/readout_board/etl_test_fw.vhd"))
             ("ipbus"
              ("ipbus-firmware/components/ipbus_core/firmware/hdl/ipbus_package.vhd"
               "registers/ipbus_decode_etl_test_fw.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/ipbus_arb.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/ipbus_fabric_sel.vhd"
               "ipbus-firmware/components/ipbus_util/firmware/hdl/ipbus_clock_div.vhd"
               "src/eth_clocks.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/ipbus_trans_decl.vhd"
               "src/eth_sgmii.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_ipaddr_block.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_rarp_block.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_build_arp.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_build_payload.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_build_ping.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_build_resend.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_build_status.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_status_buffer.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_byte_sum.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_do_rx_reset.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_packet_parser.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_rxram_mux.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_dualportram.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_buffer_selector.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_rxram_shim.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_dualportram_rx.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_dualportram_tx.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_rxtransactor_if_simple.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_tx_mux.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_txtransactor_if_simple.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_clock_crossing_if.vhd"
               "ipbus-firmware/components/ipbus_transport_udp/firmware/hdl/udp_if_flat.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/trans_arb.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/transactor_if.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/transactor_sm.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/transactor_cfg.vhd"
               "ipbus-firmware/components/ipbus_core/firmware/hdl/transactor.vhd"
               "ipbus-firmware/components/ipbus_util/firmware/hdl/masters/ipbus_ctrl.vhd"
               "src/eth_infra.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_axi_decl.vhd"
               "ipbus-firmware/components/ipbus_pcie/firmware/hdl/pcie_int_gen_msix.vhd"
               "ipbus-firmware/components/ipbus_pcie/firmware/hdl/pcie_xdma_axi_us_if.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_transport_multibuffer_rx_dpram.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_transport_multibuffer_tx_dpram.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_transport_multibuffer_cdc.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_transport_multibuffer_if.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_transport_ram_if.vhd"
               "ipbus-firmware/components/ipbus_transport_axi/firmware/hdl/ipbus_transport_axi_if.vhd"
               "src/pcie_infra.vhd"
               "ipbus-firmware/components/ipbus_slaves/firmware/hdl/ipbus_reg_types.vhd"
               "ipbus-firmware/components/ipbus_util/firmware/hdl/ipbus_decode_ipbus_example.vhd"))
             ("lpgbt_fpga"
              ("lpgbt-fpga/lpgbtfpga_package.vhd"
               "lpgbt-fpga/uplink/descrambler_51bitOrder49.vhd"
               "lpgbt-fpga/uplink/descrambler_53bitOrder49.vhd"
               "lpgbt-fpga/uplink/descrambler_58bitOrder58.vhd"
               "lpgbt-fpga/uplink/descrambler_60bitOrder58.vhd"
               "lpgbt-fpga/lpgbtfpga_downlink.vhd"
               "lpgbt-fpga/lpgbtfpga_uplink.vhd"
               "lpgbt-fpga/uplink/fec_rsDecoderN15K13.vhd"
               "lpgbt-fpga/uplink/fec_rsDecoderN31K29.vhd"
               "lpgbt-fpga/uplink/lpgbtfpga_decoder.vhd"
               "lpgbt-fpga/uplink/lpgbtfpga_deinterleaver.vhd"
               "lpgbt-fpga/uplink/lpgbtfpga_descrambler.vhd"
               "lpgbt-fpga/downlink/lpgbtfpga_encoder.vhd"
               "lpgbt-fpga/uplink/lpgbtfpga_framealigner.vhd"
               "lpgbt-fpga/downlink/lpgbtfpga_interleaver.vhd"
               "lpgbt-fpga/uplink/lpgbtfpga_rxgearbox.vhd"
               "lpgbt-fpga/downlink/lpgbtfpga_scrambler.vhd"
               "lpgbt-fpga/downlink/lpgbtfpga_txgearbox.vhd"
               "lpgbt-fpga/downlink/rs_encoder_N7K5.vhd"))
             ("gbt_sc"
              ("gbt-sc/GBT-SC/SCA/sca_pkg.vhd"
               "gbt-sc/GBT-SC/gbtsc_top.vhd"
               "gbt-sc/GBT-SC/IC/ic_deserializer.vhd"
               "gbt-sc/GBT-SC/IC/ic_rx.vhd"
               "gbt-sc/GBT-SC/IC/ic_rx_fifo.vhd"
               "gbt-sc/GBT-SC/IC/ic_top.vhd"
               "gbt-sc/GBT-SC/IC/ic_tx.vhd"
               "gbt-sc/GBT-SC/SCA/sca_deserializer.vhd"
               "gbt-sc/GBT-SC/SCA/sca_rx.vhd"
               "gbt-sc/GBT-SC/SCA/sca_rx_fifo.vhd"
               "gbt-sc/GBT-SC/SCA/sca_top.vhd"
               "gbt-sc/GBT-SC/SCA/sca_tx.vhd"))))))
